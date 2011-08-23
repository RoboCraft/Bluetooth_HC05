#ifndef BLUETOOTH_HC05_
#define BLUETOOTH_HC05_

#include <WProgram.h>
#include <stdlib.h>
#include <inttypes.h>
#include <limits.h>

enum
{
  HC05_DEFAULT_TIMEOUT = 200,
  HC05_INQUIRY_DEFAULT_TIMEOUT = 10000,
  HC05_PAIRING_DEFAULT_TIMEOUT = 10000,
  HC05_PASSWORD_MAXLEN = 16,
  HC05_PASSWORD_BUFSIZE = HC05_PASSWORD_MAXLEN + 1,
  HC05_NAME_MAXLEN = 32,
  HC05_NAME_BUFSIZE = HC05_NAME_MAXLEN + 1,
  HC05_ADDRESS_MAXLEN = 14,
  HC05_ADDRESS_BUFSIZE = HC05_ADDRESS_MAXLEN + 1,
};

typedef uint8_t BluetoothAddress[6];
typedef void (*InquiryCallback)(const BluetoothAddress &address);

enum HC05_Mode { HC05_MODE_DATA = 0, HC05_MODE_COMMAND = 1 };
enum HC05_Role { HC05_ROLE_SLAVE = 0, HC05_ROLE_MASTER = 1, HC05_ROLE_SLAVE_LOOP = 2 };
enum HC05_InquiryMode { HC05_INQUIRY_STANDARD, HC05_INQUIRY_RSSI };
enum HC05_Parity { HC05_NO_PARITY, HC05_PARITY_ODD, HC05_PARITY_EVEN };
enum HC05_Connection { HC05_CONNECT_BOUND, HC05_CONNECT_ANY, HC05_CONNECT_SLAVE_LOOP };
enum HC05_Security { HC05_SEC_OFF, HC05_SEC_NON_SECURE,
  HC05_SEC_SERVICE, HC05_SEC_LINK, HC05_SEC_UNKNOWN };
enum HC05_Encryption { HC05_ENC_OFF, HC05_ENC_PTP, HC05_ENC_PTP_BROADCAST };

enum HC05_State
{
  HC05_INITIALIZED,
  HC05_READY,
  HC05_PAIRABLE,
  HC05_PAIRED,
  HC05_INQUIRING,
  HC05_CONNECTING,
  HC05_CONNECTED,
  HC05_DISCONNECTED,
  HC05_UNKNOWN
};

enum HC05_Result
{
  HC05_OK = 0xFF,
  HC05_FAIL = 0xFE,
  HC05_ERR_TIMEOUT = 0xFD,
  HC05_ERR_ARGUMENT = 0xFC,

  HC05_ERR_DISC_LINK_LOSS = 0xFB,
  HC05_ERR_DISC_NO_SLC = 0xFA,
  HC05_ERR_DISC_TIMEOUT = 0xF9,
  HC05_ERR_DISC_ERROR = 0xF8,

  HC05_ERR_AT_COMMAND = 0x00,
  HC05_ERR_DEFAULT_RESULT = 0x01,
  HC05_ERR_PSKEY_WRITE = 0x02,
  HC05_ERR_DEVICE_NAME_TOO_LONG = 0x03,
  HC05_ERR_NO_DEVICE_NAME = 0x04,
  HC05_ERR_NAP_TOO_LONG = 0x05,
  HC05_ERR_UAP_TOO_LONG = 0x06,
  HC05_ERR_LAP_TOO_LONG = 0x07,
  HC05_ERR_NO_PIO_MASK = 0x08,
  HC05_ERR_NO_PIO_NUMBER = 0x09,
  HC05_ERR_NO_DEVICE_TYPE = 0x0A,
  HC05_ERR_DEVICE_TYPE_TOO_LONG = 0x0B,
  HC05_ERR_NO_IAC = 0x0C,
  HC05_ERR_IAC_TOO_LONG = 0x0D,
  HC05_ERR_INVALID_IAC = 0x0E,
  HC05_ERR_NO_PASSWORD = 0x0F,
  HC05_ERR_PASSWORD_TOO_LONG = 0x10,
  HC05_ERR_INVALID_MODULE_ROLE = 0x11,
  HC05_ERR_INVALID_BAUD_RATE = 0x12,
  HC05_ERR_INVALID_STOP_BITS = 0x13,
  HC05_ERR_INVALID_PARITY_BITS = 0x14,
  HC05_ERR_DEVICE_NOT_IN_LIST = 0x15,
  HC05_ERR_SPP_NOT_INITIALIZED = 0x16,
  HC05_ERR_SPP_REINIT = 0x17,
  HC05_ERR_INQUIRY_MODE = 0x18,
  HC05_ERR_INQ_TIMEOUT_TOO_LONG = 0x19,
  HC05_ERR_NO_BLUETOOTH_ADDRESS = 0x1A,
  HC05_ERR_SECURITY_MODE = 0x1B,
  HC05_ERR_ENCRYPTION_MODE = 0x1C
};

class Bluetooth_HC05: public Print
{
public:
  Bluetooth_HC05(HardwareSerial &serial = Serial);
  ~Bluetooth_HC05();

  void begin(unsigned baud_rate = 38400, uint8_t reset_pin = 0xFF,
    uint8_t mode_pin = 0xFF, HC05_Mode mode = HC05_MODE_DATA);
  bool probe(unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  void hardReset();
  bool softReset(unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getVersion(char *buffer, size_t buffer_size,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool restoreDefaults(unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getAddress(BluetoothAddress &address, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getName(char *buffer, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setName(const char *name, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getRemoteDeviceName(char *buffer, size_t buffer_size,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getRole(HC05_Role &role, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setRole(HC05_Role role, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getDeviceClass(uint32_t &device_class, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setDeviceClass(uint32_t device_class, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getInquiryAccessCode(uint32_t &iac, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setInquiryAccessCode(uint32_t iac, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getInquiryMode(HC05_InquiryMode &inq_mode, int16_t &max_devices,
    uint8_t &max_duration, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setInquiryMode(HC05_InquiryMode inq_mode, int16_t max_devices,
    uint8_t max_duration, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getPassword(char *buffer, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setPassword(const char *password, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getSerialMode(uint32_t &speed, uint8_t &stop_bits,
    HC05_Parity &parity, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setSerialMode(uint32_t speed, uint8_t stop_bits,
    HC05_Parity parity, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getConnectionMode(HC05_Connection &connection_mode,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setConnectionMode(HC05_Connection connection_mode,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool bind(const BluetoothAddress &address, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getAddressBound(BluetoothAddress &address,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getLeds(bool &led_status, bool &led_connection, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setLeds(bool led_status, bool led_connection, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setPortState(uint8_t port_num, uint8_t port_state, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getMultiplePorts(uint16_t &port_states, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setMultiplePorts(uint16_t port_states, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getInquiryAndPagingParams(uint16_t &inquiry_interval, uint16_t &inquiry_duration,
    uint16_t &paging_interval, uint16_t &paging_duration,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setInquiryAndPagingParams(uint16_t inquiry_interval, uint16_t inquiry_duration,
    uint16_t paging_interval, uint16_t paging_duration,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getSniffParams(uint16_t &max_time, uint16_t &min_time,
    uint16_t &retry_interval, uint16_t &sniff_timeout,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setSniffParams(uint16_t max_time, uint16_t min_time,
    uint16_t retry_interval, uint16_t sniff_timeout,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool enterSniffMode(unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool exitSniffMode(unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getSecurityAndEncryption(HC05_Security &security,
    HC05_Encryption &encryption, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setSecurityAndEncryption(HC05_Security security,
    HC05_Encryption encryption, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool deleteDeviceFromList(const BluetoothAddress &address,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool deleteAllDevicesFromList(unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool findDeviceInList(const BluetoothAddress &address,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool countDevicesInList(uint8_t &device_count,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getLastAuthenticatedDevice(BluetoothAddress &address,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getState(HC05_State &state, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool initSerialPortProfile(unsigned long timeout = HC05_DEFAULT_TIMEOUT);

  bool inquire(InquiryCallback callback, unsigned long timeout = HC05_INQUIRY_DEFAULT_TIMEOUT);
  bool cancelInquiry(unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool pair(const BluetoothAddress &address, unsigned long timeout = HC05_PAIRING_DEFAULT_TIMEOUT);
  bool connect(const BluetoothAddress &address, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool disconnect(unsigned long timeout = HC05_DEFAULT_TIMEOUT);

  static bool parseBluetoothAddress(BluetoothAddress &address,
    const char *address_str, char delimiter);
  static int printBluetoothAddress(char *address_str,
    const BluetoothAddress &address, char delimiter);
  
  HC05_Result getLastError() const;

private:
  HardwareSerial *m_uart;
  unsigned long m_timeout;
  unsigned long m_ticksAtStart;

  uint8_t m_modePin;
  uint8_t m_resetPin;
  HC05_Result m_errorCode;

  bool readAddressWithCommand(BluetoothAddress &address,
    const char *command_name, unsigned long timeout);
  bool writeAddressWithCommand(const BluetoothAddress &address,
    const char *command_name, unsigned long timeout);
  bool simpleCommand(const char *command_name, const char *arg, unsigned long timeout);

  bool readOperationResult();
  void writeCommand(const char *command, const char *arg = 0);
  size_t readLine(char *buffer, size_t buffer_size);
  char *readResponseWithPrefix(char *buffer, size_t buffer_size, const char *prefix);
  static char *skipPrefix(char *str, size_t str_length, const char *prefix);

  void startOperation(unsigned long timeout);
  bool isOperationTimedOut() const;
  unsigned long operationDuration() const;

  virtual void write(uint8_t data);
};

#endif // BLUETOOTH_HC05_
