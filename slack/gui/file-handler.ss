;; -*- Gerbil -*-
;; File upload and download handling for the GUI

(import
  :std/sugar
  :gerbil-qt/qt
  :ober/slack/types
  :ober/slack/api/files
  :ober/slack/gui/app
  :ober/slack/gui/channel-view
  :ober/slack/gui/theme)

(export #t)

;;;; File Upload

(def (gui-upload-file!)
  "Open file picker and upload to current channel."
  (when *current-channel-id*
    (let ((path (qt-file-dialog-open-file *main-window*)))
      (when (and path (> (string-length path) 0))
        (set-status! (string-append "Uploading " path "..."))
        (try
          (files-upload path channels: *current-channel-id*)
          (set-status! "File uploaded")
          (catch (e)
            (set-status! "Upload failed")))))))

;;;; File Download

(def (gui-download-file! file-id)
  "Download a file by ID. Opens save dialog."
  (try
    (let ((f (files-info file-id)))
      (when f
        (let* ((name (or (file-info-name f) "download"))
               (dest (qt-file-dialog-save-file *main-window*)))
          (when (and dest (> (string-length dest) 0))
            (let ((url (file-info-url-private f)))
              (when url
                (set-status! (string-append "Downloading " name "..."))
                (file-download url dest)
                (set-status! (string-append "Downloaded: " dest))))))))
    (catch (e)
      (set-status! "Download failed"))))
