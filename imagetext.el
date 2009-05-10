;;; imagetext.el --- show text parts of image files

;; Copyright 2006, 2007, 2008 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 4
;; Keywords: multimedia
;; URL: http://www.geocities.com/user42_kevin/imagetext/index.html

;; imagetext.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; imagetext.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses>.


;;; Commentary:

;; This code extends `image-mode' in Emacs 22 and `auto-image-file-mode' in
;; Emacs 21 and 22 to show text comment parts of PNG, JPEG and GIF files.
;; The image size is shown too (for all image types).
;;
;; There's no way to edit the image text, but of course there's no way to
;; edit the image itself either.
;;
;; Caution: The plain image modes don't change the buffer contents, so you
;; can save under a different filename.  But the text added here breaks
;; that.  In Emacs 22 `image-mode' if you switch back to raw with the usual
;; C-c C-c then the text extras are removed too, and hopefully it should
;; work to save from there.

;;; Install:

;; For auto-image-file-mode, put the following in your .emacs.  Note for
;; Emacs 21 you also need a copy of bindat.el (from emacs 22, it works in
;; emacs 21 unmodified),
;;
;;     (eval-after-load "image-file" '(require 'imagetext))
;;
;; For Emacs 22 image-mode, put the following in your .emacs,
;;
;;     (autoload 'imagetext-show "imagetext")
;;     (add-hook 'image-mode-hook 'imagetext-show)
;;
;; image-mode.el can work in Emacs 21 too actually, with some gentle
;; persuasion.

;;; History:

;; Version 1 - the first version.
;; Version 2 - add tiff, fix typo in png iTXt.
;; Version 3 - add tumme/image-dired comments.
;; Version 4 - lose some stray development bits


;;; Code:

(require 'bindat)


;;-----------------------------------------------------------------------------
;; misc

(defun imagetext-bindat-nulterm ()
  "Pick out a nul-terminated string for a bindat specification.
For example

    (my-asciz-field   eval (imagetext-bindat-nulterm))

The terminating 0 byte is skipped, and not included in the string
returned as the field value."

  ;; this implementation only for strings
  (let ((zpos (or (string-match "\000" bindat-raw bindat-idx)
                  (error "No null terminator"))))
    (prog1 (substring bindat-raw bindat-idx zpos)
      (setq bindat-idx (1+ zpos)))))


;;-----------------------------------------------------------------------------
;; image strings

;; The concept here, as of now, is just to pick out text parts of an image
;; file, things like title, author, copyright information etc.
;;
;; The pieces are shown in the order they're found in the file.  PNG and GIF
;; allow text in any order, so perhaps there'll be some meaning to it.
;; Though TIFF (including JPEG EXIF) is supposed to be sorted by tag code,
;; so there's no particular significance there.
;;
;; There's lots more information which could be shown, things like colour
;; spectrum, compression, scan directions, but it starts to get very
;; technical.  Perhaps a kind of priority level could be assigned if there
;; was lots more.  For now leave it to the heavy duty programs like
;; imagemagick, exiftool, image-metadata-jpeg, etc, to go into details.
;;
;; Crunching image formats in lisp might look a bit like hard work, but it's
;; also much more flexible than creating a mechanism and formats for getting
;; stuff up from the C code image libraries.  If there was any editing of
;; the info in the future it'd be different, you'd probably want the image
;; libraries to do that.
;;

(defun imagetext-strings (image raw)
  "Extract text comments from an image.
IMAGE is an image descriptor, RAW is a unibyte string of the data.
The return is a list of strings describing the elements found."

  (let ((type (plist-get (cdr image) :type)))
    (condition-case err
        (cond ((eq type 'png)  (imagetext-png-strings raw))
              ((eq type 'gif)  (imagetext-gif-strings raw))
              ((eq type 'jpeg) (append (imagetext-size-strings image)
                                       (imagetext-jpeg-strings raw)))
              ((eq type 'tiff) (append (imagetext-size-strings image)
                                       (imagetext-tiff-strings raw))))
      (error (list "Invalid or unrecognised image file contents\n"
                   (error-message-string err))))))

(defun imagetext-size-strings (image)
  "Return a list of strings representing the size of IMAGE.
IMAGE is an image descriptor, or a warning string if not displayable."

  ;; `image-size' throws an error on a non-gui display, which is a shame
  ;; because the image libraries can give the info without displaying
  (let ((size (condition-case nil (image-size image t) (error nil))))
    (and size
         (list (format "Size %dx%d\n" (car size) (cdr size))))))


;;-----------------------------------------------------------------------------
;; png bits

;; Crib notes:
;;
;; tEXt has a latin-1 keyword and text, there's some standard keywords but
;; anything is possible.  The keyword is actually a subset of latin-1, with
;; non-break space and a few other chars disallowed, but that doesn't matter
;; here.
;;
;; zTXt is tEXt with zlib format compression.
;;
;; iTXt has latin-1 keyword (same as tEXt), a language tag, a utf-8
;; translated keyword, and utf-8 text.  There's an optional zlib compression
;; too.

(defun imagetext-png-strings (raw)
  "Extract text comments from PNG image data.
RAW in the image data as a unibyte string, the return is a list
of text strings found (multibyte strings)."

  (let ((pos 8)
        ret)
    (while (< pos (length raw))
      ;; chunk
      (let* ((struct (bindat-unpack '((:length    u32)
                                      (:type      str 4)
                                      (:data      str (:length))
                                      (:crc       str 4)
                                      ((eval (setq pos bindat-idx))))
                                    raw pos))
             (type   (bindat-get-field struct :type))
             (data   (bindat-get-field struct :data)))

        (if nil ;; diagnostic message, disabled
            (push (format "%s: %s bytes\n" type (length data)) ret))

        (cond
         ((string-equal type "IHDR")
          (let* ((struct (bindat-unpack '((:width  u32)
                                          (:height u32)) data)))
            (push (format "Size %dx%d\n"
                          (bindat-get-field struct :width)
                          (bindat-get-field struct :height))
                  ret)))

         ((string-equal type "tEXt")
          (let* ((struct (bindat-unpack
                          '((:keyword eval (imagetext-bindat-nulterm))
                            (:text    str  (eval (- (length bindat-raw)
                                                    bindat-idx))))
                          data)))
            (push (format "%s: %s\n"
                          (decode-coding-string
                           (bindat-get-field struct :keyword) 'latin-1)
                          (decode-coding-string
                           (bindat-get-field struct :text) 'latin-1)) ret)))

         ((string-equal type "zTXt")
          (let* ((struct (bindat-unpack
                          '((:keyword  eval (imagetext-bindat-nulterm))
                            (:method   u8)
                            (:comptext str (eval (- (length bindat-raw)
                                                    bindat-idx))))
                          data)))
            (push (format "%s: %s\n"
                          (decode-coding-string
                           (bindat-get-field struct :keyword) 'latin-1)
                          (decode-coding-string
                           (imagetext-png-zTXt-inflate
                            (bindat-get-field struct :method)
                            (bindat-get-field struct :comptext))
                           'latin-1)) ret)))

         ((string-equal type "iTXt")
          (let* ((struct (bindat-unpack
                          '((:keyword  eval (imagetext-bindat-nulterm))
                            (:compflag u8)
                            (:method   u8)
                            (:lang     eval (imagetext-bindat-nulterm))
                            (:lkeyword eval (imagetext-bindat-nulterm))
                            (:text     str  (eval (- (length bindat-raw)
                                                     bindat-idx))))
                          data))
                 (text   (bindat-get-field struct :text)))
            (if (= 1 (bindat-get-field struct :compflag))
                (setq text (imagetext-png-zTXt-inflate
                            (bindat-get-field struct :method) text)))
            (push (format "%s %s %s: %s\n"
                          (decode-coding-string
                           (bindat-get-field struct :keyword) 'latin-1)
                          (decode-coding-string ;; supposed to be ascii
                           (bindat-get-field struct :lang) 'undecided)
                          (decode-coding-string
                           (bindat-get-field struct :lkeyword) 'utf-8)
                          (decode-coding-string text 'utf-8))
                  ret)))

         ((string-equal type "tIME")
          (let* ((struct   (bindat-unpack '((:year   u16)
                                            (:month  u8)
                                            (:day    u8)
                                            (:hour   u8)
                                            (:minute u8)
                                            (:second u8)) data)))
            (push (format "%s: %d-%02d-%02d %02d:%02d:%02d\n"
                          type
                          (bindat-get-field struct :year)
                          (bindat-get-field struct :month)
                          (bindat-get-field struct :day)
                          (bindat-get-field struct :hour)
                          (bindat-get-field struct :minute)
                          (bindat-get-field struct :second))
                  ret))))))
    (nreverse ret)))

(defun imagetext-png-zTXt-inflate (method data)
  "Inflate a PNG compresed data string.
METHOD is the integer method code, but only 0 for \"inflate\" is
supported, for others a warning message string is returned.
DATA is a unibyte string and on success the return is likewise a
unibyte string."
  (cond ((= method 0)
         (imagetext-inflate data))
        (t
         (format "<unknown compression method %s>" method))))

(defun imagetext-inflate (str)
  "Inflate Zlib format (RFC 1950) compressed data STR.
STR should be unibyte and the return is similarly a unibyte string.

This is implemented by running the gzip program, which is pretty
nasty since usually Emacs has zlib linked in already (used by
libpng) so one day there might be a direct interface to it."

  (let* ((flg   (aref str 1))
         (fdict (logand flg #x20))
         (cm    (logand #x0F (aref str 0))))
    (if (= 01 fdict)
        "<Zlib FDICT pre-defined dictionary not supported>"

      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert (string 31 139  ;; ID1,ID2
                        cm      ;; CM compression method
                        0       ;; FLG flags
                        0 0 0 0 ;; MTIME
                        0       ;; XFL extra flags
                        3))     ;; OS = Unix
        (insert (substring str 2)) ;; drop CMF and FLG
        (insert (string 0 0 0 0)) ;; ISIZE faked
        (let* ((coding-system-for-read  'no-conversion)
               (coding-system-for-write 'no-conversion)
               (status (call-process-region (point-min) (point-max) "gzip"
                                            t '(t nil) nil "-d")))
          ;; report if died by signal, other errors are expected because we
          ;; leave the zlib ADLER32 checksum pretending to be CRC32 (wrong
          ;; of course), and the ISIZE uncompressed size is faked
          (when (stringp status)
            (goto-char (point-min))
            (insert (format "<gzip: %s>" status))))
        (buffer-string)))))


;;-----------------------------------------------------------------------------
;; jpeg bits

;; Crib notes:
;;
;; Two bytes, the first of which is #xFF, are a marker.  A marker can be
;; alone or can have a 2-byte length which is a segment of meta info.
;; Actual image data is in the form of ECS "entropy coded stream" in between
;; such markers or segments.  ECS is meant to be only at certain places, but
;; we don't worry about that, it can be just skipped.  ECS doesn't contain
;; any #xFF bytes, except as escapes #xFF #xFF or #xFF #x00 and those two
;; can be treated as markers without segment length.
;;
;; "APP0" #xFF #xE0 segment has a format ID and version.  Textual comments
;; are in a "COM" #xFF #xFE segment.

(defun imagetext-jpeg-strings (raw)
  "Extract text comments from JPEG image data.
RAW in the image data as a unibyte string, the return is a list
of text strings found (multibyte strings)."

  (let ((pos 0)
        ret)

    ;; skip to FF each time, to pass over ECS data
    (while (setq pos (string-match "\377" raw pos))

      (let* ((struct (bindat-unpack
                      '((:marker u16)
                        (union (eval last)
                               ;; escapes in ECS treated as marker only
                               (#xFF00) (#xFFFF)
                               ;; RST0 through RST7, marker only
                               (#xFFD0) (#xFFD1) (#xFFD2) (#xFFD3)
                               (#xFFD4) (#xFFD5) (#xFFD6) (#xFFD7)
                               ;; SOI and EOI, marker-only
                               (#xFFD8) (#xFFD9)
                               ;; otherwise length and data
                               (t (:length u16)
                                  (:data   str (eval (- last 2)))))
                        ((eval (setq pos bindat-idx))))
                      raw pos))
             (marker (bindat-get-field struct :marker))
             (data   (bindat-get-field struct :data)))

        (if nil ;; diagnostic message, disabled
            (push (format "%x: %s bytes\n" marker (length data)) ret))

        (cond ((= #xFFD9 marker)  ;; EOI
               ;; stop, in case garbage after
               (setq pos (length raw)))

              ((= #xFFE0 marker)  ;; APP0
               (if (or (eq t (compare-strings data 0 4 "JFIF" 0 4))
                       (eq t (compare-strings data 0 4 "JFXX" 0 4)))
                   (let* ((struct (bindat-unpack '((:ident         str 4)
                                                   (:null          u8)
                                                   (:major-version u8)
                                                   (:minor-version u8))
                                                 data)))

                     (push (format "%s version %d.%02d\n"
                                   (bindat-get-field struct :ident)
                                   (bindat-get-field struct :major-version)
                                   (bindat-get-field struct :minor-version))
                           ret))))

              ((= #xFFE1 marker)  ;; APP1
               (if (eq t (compare-strings data 0 6 "Exif\000\000" 0 6))
                   ;; exif is a segment of tiff data, including the usual
                   ;; tiff header
                   (setq ret (nconc (nreverse (imagetext-tiff-strings
                                               (substring data 6)))
                                    ret))))

              ((= #xFFFE marker)  ;; COM comment
               ;; dunno what the text encoding should be, let emacs guess
               (push (format "%s\n"
                             (decode-coding-string data 'undecided))
                     ret)))))
    (nreverse ret)))


;;-----------------------------------------------------------------------------
;; tiff bits

(defun imagetext-tiff-strings (raw)
  "Extract text comments from TIFF image data.
RAW in the image data as a unibyte string, the return is a list
of text strings found (multibyte strings)."

  (let* (ret ifdpos X-u16 X-u32)

    ;; 8-byte header
    ;; X-u16 setup as either 'u16 or 'u16r, according to the endianess, and
    ;; likewise X-u32
    (let* ((struct (bindat-unpack '((:endian  str 2)
                                    ((eval (cond ((string-equal "MM" last)
                                                  (setq X-u16 'u16)
                                                  (setq X-u32 'u32))
                                                 ((string-equal "II" last)
                                                  (setq X-u16 'u16r)
                                                  (setq X-u32 'u32r)))))
                                    (:mark42  (eval X-u16))
                                    (:ifdpos  (eval X-u32)))
                                  raw)))
      (setq ifdpos (bindat-get-field struct :ifdpos)))

    ;; loop looking at all IFDs in the file
    ;; the second and subsequent are supposed to be about sub-images or
    ;; something, so maybe ought to identify that somehow
    (while (/= 0 ifdpos)

      ;; The count field is followed by 4 bytes which are either the field
      ;; data there inline, or a 32-bit file position of the data.  Inline
      ;; is used when there's <= 4 bytes in the field.  We test only
      ;; count<=4 because that's enough for the ascii (count is bytes)
      ;; fields we're interested in.  (And we're safe if ever u32 decode got
      ;; some overflow checking, because we err in treating some remotes as
      ;; inline; any u32 decode is certainly a file offset.)

      (let* ((entry-spec '((:tag       (eval X-u16))
                           (:type      (eval X-u16))
                           (:count     (eval X-u32))
                           (union (eval last)
                                  ((eval (<= tag 4))
                                   (:datapos eval bindat-idx)  ;; inline
                                   (         fill 4))
                                  (t
                                   (:datapos (eval X-u32)))))) ;; remote
             (struct (bindat-unpack '((:numentries (eval X-u16))
                                      (:entries    repeat (:numentries)
                                                   (struct entry-spec))
                                      (:nextifd    (eval X-u32)))
                                    raw ifdpos)))

        ;; The alist is the tags to actually show, and only ascii ones
        ;; supported.
        ;; - #x13C "HostComputer" is not shown because that seems very
        ;;   irrelevant.
        ;; - #x131 "Software" is shown; it's of doubtful interest, but in
        ;;   formats like PNG that kind of info shows up, so have it here
        ;;   for consistency.
        ;; - #x10F "Make" and #x110 "Model" for the camera are
        ;;   possibilities, but would seem of very limited interest
        ;;
        (dolist (entry (bindat-get-field struct :entries))
          (let* ((tag     (bindat-get-field entry :tag))
                 (tagname (cdr (assoc tag
                                      '((#x10D  . "DocumentName")
                                        (#x10E  . "ImageDescription")
                                        (#x11D  . "PageName")
                                        (#x131  . "Software")
                                        (#x132  . "DateTime")
                                        (#x13B  . "Artist")
                                        (#x8298 . "Copyright"))))))

            (if nil ;; diagnostic message, disabled
                (push (format "tag %x\n" tag) ret))

            (when (and tagname
                       (= 2 (bindat-get-field entry :type))) ;; ASCII

              ;; The value offset field is a 32-bit file position, except if
              ;; the field is <= 4 bytes, in which case the bytes are inline
              ;; there directly.  The size of each count element varies
              ;; according to the type, so we don't know how many bytes
              ;; until identifying the type field, in this case ASCII data
              ;; which means simply count bytes.  (Want to avoid attempting
              ;; a u32 decode until being sure it's really an offset, in
              ;; case it's some strange bytes overflowing the conversion.)
              ;;
              (let* ((count   (bindat-get-field entry :count))
                     (datapos (bindat-get-field entry :datapos))
                     (data    (substring raw datapos (+ datapos count))))

                ;; There's always a trailing \0, then any \0's in the middle
                ;; separate multiple values such as multiple copyright
                ;; holders in a #x8298 field.  Ascii fields are supposed to
                ;; be ascii, but let's decode as 'undecided just in case
                ;; there's something zany.
                ;;
                (setq data (replace-regexp-in-string "\000\\'" "" data t t))
                (setq data (decode-coding-string data 'undecided))
                (dolist (str (split-string data "\000"))
                  (push (format "%s: %s\n"  tagname str) ret))))))

        (setq ifdpos (bindat-get-field struct :nextifd))
        (if (/= 0 ifdpos)
            (push "\nSubfile:\n" ret))))

    ret))


;;-----------------------------------------------------------------------------
;; gif bits

(defun imagetext-gif-strings (raw)
  "Extract text comments from GIF image data.
RAW is the image data as a unibyte string, the return is a list
of text strings found (multibyte strings)."

  (let* ((pos 0)
         ret)

    ;; header
    (let* ((struct   (bindat-unpack '((:sig+ver      str 6)
                                      (:width        u16r)
                                      (:height       u16r)
                                      (flags        u8)
                                      (background   u8)
                                      (aspect-ratio u8)
                                      ((eval (setq pos bindat-idx))))
                                    raw))
           (flags    (bindat-get-field struct 'flags))
           (gct-flag (= #x80 (logand #x80 flags)))
           (gct-size (logand #x07 flags)))

      ;; global colour table 3*2^(gctsize+1) bytes, when flag set
      (if gct-flag
          (setq pos (+ pos (* 3 (ash 2 gct-size)))))

      (push (format "%s, size %dx%d\n"
                    (bindat-get-field struct :sig+ver)
                    (bindat-get-field struct :width)
                    (bindat-get-field struct :height))
            ret))

    (while (< pos (length raw))
      (let* ((type (aref raw pos)))
        (setq pos (1+ pos))

        (cond ((= #x3B type) ;; trailer
               )

              ((= #x2C type)  ;; image descriptor
               (let* ((struct (bindat-unpack '((left   u16r)
                                               (top    u16r)
                                               (:width  u16r)
                                               (:height u16r)
                                               (flags  u8)
                                               ((eval (setq pos bindat-idx))))
                                             raw pos))
                      (flags    (bindat-get-field struct 'flags))
                      (lct-flag (= #x80 (logand #x80 flags)))
                      (lct-size (logand #x07 flags)))
                 ;; local colour table 3*2^(lctsize+1) bytes, when flag set
                 (if lct-flag
                     (setq pos (+ pos (* 3 (ash 2 lct-size)))))

                 ;; table data
                 (setq pos (1+ pos)) ;; LZW minimum code size
                 ;; data blocks, first byte is length, stop at 0 len
                 (while (let ((blocklen (aref raw pos)))
                          (setq pos (+ pos 1 blocklen))
                          (/= 0 blocklen)))))

              ((= #x21 type)  ;; extension
               (setq type (aref raw pos))
               (setq pos (1+ pos))

               (let ((data ""))
                 ;; concat data blocks, first byte is length, stop at 0 len
                 (while (let ((blocklen (aref raw pos)))
                          (setq data (concat data
                                             (substring raw (1+ pos)
                                                        (+ pos 1 blocklen))))
                          (setq pos (+ pos 1 blocklen))
                          (/= 0 blocklen)))

                 (cond ((= #xFE type) ;; comment
                        ;; supposed to be 7-bit ascii, attempt a decode in case
                        (push (format "%s\n"
                                      (decode-coding-string data 'undecided))
                              ret))))))))
    (nreverse ret)))


;;-----------------------------------------------------------------------------
;; image text insertions

(defun imagetext-insert-after-image (filename)
  "Insert text information for an image at point.
The image should be at point in the form of a display property on
the raw bytes.  Text from the image is inserted immediately after
that.

FILENAME is the originating filename (or nil if unknown), which
is used to get tumme/image-dired comments."

  (let ((image (get-text-property (point) 'display)) ;; image descriptor
        (modified (buffer-modified-p))
        (inhibit-read-only t)) ;; avoid read-only on image
    (when image
      (unwind-protect
          (save-excursion
            (save-restriction
              ;; the image data part
              (narrow-to-region (point) (or (next-property-change (point))
                                            (point-max)))

              ;; the following leaves the buffer alone and builds text parts
              ;; in a list, since obviously don't want to change the buffer
              ;; contents until everything picked out, and also switch back
              ;; to multibyte until the end
              ;;
              (set-buffer-multibyte nil)
              (let ((lst (imagetext-strings
                          image (buffer-substring-no-properties (point-min)
                                                                (point-max)))))

                ;; changing from unibyte to multibyte makes a mess of the
                ;; image property coverage, restore it
                (let ((props (text-properties-at (point-min))))
                  (set-buffer-multibyte t)
                  (set-text-properties (point-min) (point-max) props))

                ;; now actually insert the text parts
                (goto-char (point-max))
                (insert "\n\n")
                (when filename
                 (insert (imagetext-tumme-string filename)))
                (mapc 'insert lst))))

        ;; if an error occurs while making our insertions still consider
        ;; unmodified
        (restore-buffer-modified-p modified)))))

(defun imagetext-tumme-string (filename)
  "Get tumme/image-dired comment and tags for FILENAME.
The return is a string, empty if nothing recorded or
tumme/image-dired not available."
  ;; called tumme in its standalone distribution, but called image-dired in
  ;; emacs 22, and allow for neither available in emacs 21
  (let ((ret "")
        comment taglist)
    (cond ((or (featurep 'image-dired)
               (and (locate-library "image-dired")
                    (require 'image-dired)))
           (setq comment (image-dired-get-comment filename))
           (setq taglist (image-dired-list-tags filename))
           (setq ret "image-dired: "))

          ((or (featurep 'tumme)
               (and (locate-library "tumme")
                    (require 'tumme)))
           (setq comment (tumme-get-comment filename))
           (setq taglist (tumme-list-tags filename))
           (setq ret "tumme: ")))

    ;; single "" when no tags in emacs 22
    (setq taglist (remove "" taglist))

    (if (or comment taglist)
        (let ((sep ""))
          (when comment
            (setq ret (concat ret comment))
            (setq sep "; "))
          (mapc (lambda (tag)
                  (setq ret (concat ret sep tag))
                  (setq sep "; "))
                taglist)
          (concat ret "\n"))
      ;; no comment or tags
      "")))


;;-----------------------------------------------------------------------------
;; extending image-mode

(defvar imagetext-marker nil
  "A marker for where the image ends and the extra text begins.")
(make-variable-buffer-local 'imagetext-marker)

(defun imagetext-show ()
  "Show text fields from a PNG file.
This function adds the fields when `image-mode' is showing the image as an
image, or removes them if it's showing raw bytes."

  (when (display-images-p) ;; image-mode does nothing if images not displayable
    (let ((inhibit-read-only t)
          (modified (buffer-modified-p))
          (image    (get-text-property (point-min) 'display)))

      ;; delete old bits, if any
      (when imagetext-marker
        (delete-region imagetext-marker (point-max))
        (setq imagetext-marker nil))
      (set-buffer-multibyte nil)

      ;; insert new bits, if displaying as image
      (when image
        (setq imagetext-marker (point-max-marker))
        (goto-char (point-min))
        (imagetext-insert-after-image (buffer-file-name))
        (setq cursor-type t))

      (set-buffer-modified-p modified))))

(defadvice image-toggle-display (after imagetext activate)
  "Show text fields from PNG, JPEG and GIF files."
  (imagetext-show))


;;-----------------------------------------------------------------------------
;; extending auto-image-file-mode

(defadvice insert-image-file (after imagetext
                                    (file &optional visit beg end replace)
                                    activate)
  "Show text fields from PNG, JPEG and GIF files."
  ;; same condition as image-file.el tests
  (when (and (or (null beg) (zerop beg)) (null end))
    (imagetext-insert-after-image file)))


(provide 'imagetext)

;;; imagetext.el ends here
