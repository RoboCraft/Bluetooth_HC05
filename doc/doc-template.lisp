(defmacro keyword-style (arg)
  `(span :class "keyword" ,arg))

(defmacro operator-style (arg)
  `(span :class "operator" ,arg))

(defmacro const-style (arg)
  `(span :class "constant" ,arg))

(defmacro num-style (arg)
  `(span :class "number" ,arg))

(defmacro type-style (arg)
  `(span :class "type" ,arg))

(defmacro method-style (arg)
  `(span :class "method" ,arg))

(defmacro chinese-warning ()
  `(span :class "chinese-warning" "狗屁"))

(defmacro line (&body body)
  `(progn ,@body (br)))

(defmacro lines (&body body)
  `(progn
    ,@(loop for ln in body collecting `(line ,ln))))

(defmacro desc (&body body)
  `(line (blockquote ,@body)))

(defconstant +indent+ "  ")
(defmacro >> () `(format t +indent+))

(defmacro hlink (address text)
  `(lml-format "<a href=\"~a\">~a</a>" ,address ,text))

(defmacro hlink-from-pair (pair)
  `(hlink (cdr ,pair) (car ,pair)))

(defmacro description-with-link (description link-pair)
 `(html-string
    (lml-format ,description
      (html-string (hlink-from-pair ,link-pair)))))

(defmacro section (name &body body)
  `(h2 (a :name ,name ,@body)))

(defmacro const-description (name description)
 `(p (const-style ,name) (br) ,description))

(defmacro const-descriptions (&rest pairs)
 `(progn
  ,@(loop for pair in pairs collecting
      (if (symbolp pair)
       `(const-description ,(symbol-name pair) ,pair)
       `(const-description ,(first pair) ,(second pair))))))

(defmacro enum-description (name description &body body)
 `(p(div :class "enum-description"
    (line (type-style ,name))
    (princ ,description)
    (blockquote
      (const-descriptions ,@body)))))

(defmacro style (&rest args)
  `(with style ,@args))

(defmacro html-string (&body body)
 `(with-output-to-string (lml:*html-output*) ,@body))

(defmacro html-format (format-string &rest args)
 `(lml-format ,format-string
  ,@(loop for arg in args collecting
     `(with-output-to-string (lml:*html-output*) ,arg))))

(defmacro html-format-to-string (format-string &rest args)
 `(with-output-to-string (lml:*html-output*)
    (html-format ,format-string ,@args)))

(defmacro method-description (method description)
 `(html-format "~a: ~a" (method-style ,method) (lml-princ ,description)))

(defmacro const-description-symbol (symbol)
  `(const-description (,(symbol-name symbol) ,symbol)))

(defconstants
  (+xml-prologue-string+ "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>")
  (+xhtml-prologue-string+ "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml10-strict.dtd\">"))

(lml-format "~A~%" +xml-prologue-string+)
(lml-format "~A~%" +xhtml-prologue-string+)
(html :xmlns "http://www.w3.org/1999/xhtml"
  (head
    (title +title+)
    (meta :http-equiv "Content-Type" :content "text/html" :charset "utf-8")

    (style :type "text/css"
      (cssexp:with-css-output (*standard-output*)
        (:.keyword
          :color "#00007f"
          :font-weight "bold")
        (:.operator :color "#301010")
        (:.constant :color "#663300")
        (:.number :color "#007f00")
        (:.type
          :color "#2b71fd"
          :font-weight "bold")
        (:.method
          :color "#555533"
          :font-weight "bold")
          ;:font-style "italic")
        (:.enum-description
          :border "2px solid #aaaaaa"
          :padding "0.5em")
        (:.chinese-warning
          :margin "5px 5px"
          :border "2px solid #ff4400"))))

  (body
    (h1 +top-header+)
    (p (lml-format +synopsis+ (html-string (chinese-warning))))

    ;; Table of contents
    (ol (li (hlink "#constants" +constants-header+))
        (li (hlink "#types" +types-header+)))

    (section "constants" +constants-header+)

    (const-descriptions
      HC05_DEFAULT_TIMEOUT
      HC05_INQUIRY_DEFAULT_TIMEOUT
      HC05_PAIRING_DEFAULT_TIMEOUT
      HC05_PASSWORD_MAXLEN
      HC05_PASSWORD_BUFSIZE
      HC05_NAME_MAXLEN
      HC05_NAME_BUFSIZE
      HC05_ADDRESS_MAXLEN
      HC05_ADDRESS_BUFSIZE)

    (section "types" +types-header+)

    (p (line (type-style "BluetoothAddress")) +BluetoothAddress+)

    (enum-description "HC05_Mode" HC05_Mode
      HC05_MODE_DATA HC05_MODE_COMMAND)

    (enum-description "HC05_Role" HC05_Role
      HC05_ROLE_SLAVE HC05_ROLE_MASTER HC05_ROLE_SLAVE_LOOP)

    (enum-description "HC05_InquiryMode" HC05_InquiryMode
      HC05_INQUIRY_STANDARD
      ("HC05_INQUIRY_RSSI"
        (lml-format HC05_INQUIRY_RSSI
           (html-string (hlink-from-pair +RSSI-wikipedia-link+)))))

    (enum-description "HC05_Parity"
      (description-with-link HC05_Parity +parity-bit-wikipedia-link+)
      HC05_NO_PARITY
      HC05_PARITY_ODD
      HC05_PARITY_EVEN)

    (enum-description "HC05_Connection" HC05_Connection
      ("HC05_CONNECT_BOUND" (span HC05_CONNECT_BOUND (method-style "bind()")))
      HC05_CONNECT_ANY
      ("HC05_CONNECT_SLAVE_LOOP" (span HC05_CONNECT_SLAVE_LOOP
                                       (chinese-warning))))
    (defconstant +security-link+ "http://www.palowireless.com/bluearticles/cc1_security1.asp")

    (enum-description "HC05_Security"
      (html-format-to-string HC05_Security (hlink +security-link+ +security-link-name+))
      ("HC05_SEC_OFF" (span HC05_SEC_OFF (chinese-warning)))
      HC05_SEC_NON_SECURE HC05_SEC_SERVICE HC05_SEC_LINK
      ("HC05_SEC_UNKNOWN" (span HC05_SEC_UNKNOWN (chinese-warning))))

    (enum-description "HC05_Encryption"
      (html-format-to-string HC05_Encryption
        (hlink +security-link+ +security-link-name+)
        (b +packet-encryption-section+))
      HC05_ENC_OFF HC05_ENC_PTP HC05_ENC_PTP_BROADCAST)

    (enum-description "HC05_State" HC05_State
      HC05_INITIALIZED
      ("HC05_READY" (span HC05_READY (chinese-warning)))
      HC05_PAIRABLE HC05_PAIRED HC05_INQUIRING
      HC05_CONNECTING HC05_CONNECTED HC05_DISCONNECTED
      ("HC05_UNKNOWN" (span HC05_UNKNOWN (chinese-warning))))

    (enum-description "HC05_Result" HC05_Result
      HC05_OK
      ("HC05_FAIL" (span HC05_FAIL (chinese-warning)))
      HC05_ERR_TIMEOUT HC05_ERR_ARGUMENT HC05_ERR_DISC_LINK_LOSS
      ("HC05_ERR_DISC_NO_SLC" (chinese-warning))
      ("HC05_ERR_DISC_TIMEOUT" (span HC05_ERR_DISC_TIMEOUT (chinese-warning)))
      ("HC05_ERR_DISC_ERROR" (span HC05_ERR_DISC_ERROR (chinese-warning)))
      HC05_ERR_AT_COMMAND
      ("HC05_ERR_DEFAULT_RESULT" (span HC05_ERR_DEFAULT_RESULT (chinese-warning)))
      HC05_ERR_PSKEY_WRITE HC05_ERR_DEVICE_NAME_TOO_LONG HC05_ERR_NO_DEVICE_NAME
      HC05_ERR_NAP_TOO_LONG HC05_ERR_UAP_TOO_LONG HC05_ERR_LAP_TOO_LONG
      HC05_ERR_NO_PIO_MASK HC05_ERR_NO_PIO_NUMBER HC05_ERR_NO_DEVICE_TYPE
      HC05_ERR_DEVICE_TYPE_TOO_LONG
      HC05_ERR_NO_IAC HC05_ERR_IAC_TOO_LONG HC05_ERR_INVALID_IAC HC05_ERR_NO_PASSWORD
      HC05_ERR_PASSWORD_TOO_LONG HC05_ERR_INVALID_MODULE_ROLE HC05_ERR_INVALID_BAUD_RATE
      HC05_ERR_INVALID_STOP_BITS HC05_ERR_INVALID_PARITY_BITS HC05_ERR_DEVICE_NOT_IN_LIST
      HC05_ERR_SPP_NOT_INITIALIZED HC05_ERR_SPP_REINIT HC05_ERR_INVALID_INQUIRY_MODE
      HC05_ERR_INQUIRY_TIMEOUT_TOO_LONG HC05_ERR_NO_BLUETOOTH_ADDRESS
      HC05_ERR_SECURITY_MODE HC05_ERR_ENCRYPTION_MODE)))
