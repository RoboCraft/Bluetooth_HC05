#ifndef BLUETOOTH_HC05_
#define BLUETOOTH_HC05_

#include <WProgram.h>
#include <stdlib.h>
#include <inttypes.h>
#include <limits.h>

enum
{
  HC05_DEFAULT_TIMEOUT = 100,
  HC05_PASSWORD_MAXLEN = 16,
  BLUETOOTH_ADDRESS_MAXLEN = 14
};

enum HC05_Mode { HC05_MODE_DATA = 0, HC05_MODE_COMMAND = 1 };
enum HC05_Role { HC05_ROLE_SLAVE = 0, HC05_ROLE_MASTER = 1, HC05_ROLE_SLAVE_LOOP = 2 };
enum HC05_InquiryMode { HC05_INQUIRY_STANDARD, HC05_INQUIRY_RSSI };
enum HC05_Parity { HC05_NO_PARITY, HC05_PARITY_ODD, HC05_PARITY_EVEN };
enum HC05_Connection { HC05_CONNECT_BOUND, HC05_CONNECT_ANY, HC05_CONNECT_SLAVE_LOOP };

enum HC05_Result
{
  HC05_OK = 0xFF,
  HC05_FAIL = 0xFE,
  
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
  HC05_ERR_INVALID_INQUIRY_MODE = 0x18,
  HC05_ERR_INQUIRY_TIMEOUT_TOO_LONG = 0x19,
  HC05_ERR_NO_BLUETOOTH_ADDRESS = 0x1A,
  HC05_ERR_INVALID_SECURITY_MODE = 0x1B,
  HC05_ERR_INVALID_ENCRYPTION_MODE = 0x1C,
  
  
};

typedef uint8_t BluetoothAddress[6];

extern const float HC05_INQUIRY_QUANT; // 1.28


class Bluetooth_HC05: public Print
{
public:
  Bluetooth_HC05(HardwareSerial &serial = Serial);
  ~Bluetooth_HC05();
  
  HC05_Result getLastError() const;

  void begin(unsigned baud_rate = 9600, uint8_t reset_pin = 0xFF,
    uint8_t mode_pin = 0xFF, HC05_Mode mode = HC05_MODE_DATA);
  bool probe(unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  void hardReset();
  bool softReset(unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getVersion(char *buffer, size_t buffer_size,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool restoreDefaults(unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getAddress(BluetoothAddress &address, unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool getName(char *buffer, size_t buffer_size,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
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
  bool getPagingAndInquiryParams(uint16_t &inquiry_interval, uint16_t &inquiry_duration,
    uint16_t &paging_interval, uint16_t &paging_duration,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool setPagingAndInquiryParams(uint16_t inquiry_interval, uint16_t inquiry_duration,
    uint16_t paging_interval, uint16_t paging_duration,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  
  static bool parseBluetoothAddress(BluetoothAddress &address, const char *addr_str);
  static void printBluetoothAddress(char *addr_str,
    const BluetoothAddress &address, char delimiter = ':');
  
private:
  HardwareSerial *m_uart;
  unsigned long m_timeout;
  unsigned long m_ticksAtStart; 

  uint8_t m_modePin;
  uint8_t m_resetPin;
  HC05_Result m_errorCode;
  
  bool readAddressWithCommand(BluetoothAddress &address, const char *command_name,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);
  bool writeAddressWithCommand(const BluetoothAddress &address, const char *command_name,
    unsigned long timeout = HC05_DEFAULT_TIMEOUT);

  bool readOK();
  void writeCommand(const char *command, const char *arg = 0);
  size_t readLine(char *buffer, size_t buffer_size);
  char *readResponseWithPrefix(char *buffer, size_t buffer_size, char *prefix);
  static char *skipPrefix(char *str, size_t str_length, char *prefix);
  
  void startOperation(unsigned long timeout);
  bool isOperationTimedOut() const;
  unsigned long operationDuration() const;
  
  virtual void write(uint8_t data);
};

#endif // BLUETOOTH_HC05_
