#include <quickfix/FileStore.h>
#include <quickfix/FileLog.h>
#include <quickfix/SocketInitiator.h>
#include <quickfix/Session.h>
#include <quickfix/SessionSettings.h>
#include <quickfix/Application.h>
#include  <quickfix/fix44/NewOrderSingle.h>

#include  <stdio.h>
#include  <sys/time.h>

using namespace FIX;

class MyApplication : public FIX::Application
{
public:
   virtual ~MyApplication() {};
   virtual void onCreate(const SessionID&)
   {
   }
   virtual void onLogon(const SessionID&)
   {
   }
   virtual void onLogout(const SessionID&)
   {
   }
   virtual void toAdmin(Message&, const SessionID&)
   {
   }
   virtual void toApp(Message& msg, const SessionID&) throw(DoNotSend)
   {
      FIX::ClOrdID clOrdID;
      msg.getField(clOrdID);
      timeval tv;
      gettimeofday(&tv, NULL);
      m_timestamp[clOrdID.getValue()] = tv.tv_sec * 1000000 + tv.tv_usec;
      //std::cout << "{" << clOrdID.getValue() << "," << tv.tv_sec * 1000000 + tv.tv_usec << "}" << std::endl;
   }
   virtual void fromAdmin(const Message&, const SessionID&) throw( FieldNotFound, IncorrectDataFormat, IncorrectTagValue, RejectLogon)
   {
   }
   virtual void fromApp(const Message& msg, const SessionID&) throw( FieldNotFound, IncorrectDataFormat, IncorrectTagValue, UnsupportedMessageType)
   {
      timeval tv;
      gettimeofday(&tv, NULL);
      FIX::ClOrdID clOrdID;
      msg.getField(clOrdID);
      std::cout << clOrdID << "   " << (tv.tv_sec * 1000000 + tv.tv_usec) - m_timestamp[clOrdID.getValue()] << std::endl;
   }
private:
   typedef std::map<std::string, long> Timestamp;
   Timestamp m_timestamp;
};

int main(int argc, char *argv[])
{
   try
   {
      FIX::SessionSettings settings("qfix_client.cfg");

      MyApplication application;
      FIX::FileStoreFactory storeFactory(settings);
      FIX::FileLogFactory logFactory(settings);
      FIX::SocketInitiator initiator(application, storeFactory, settings, logFactory);
      initiator.start();
      while(true)
      {
         static int val = 0;
         std::ostringstream ost;
         ost << "clordid_" << ++val;
         FIX44::NewOrderSingle msg(
            FIX::ClOrdID(ost.str()),
            FIX::Side(FIX::Side_BUY),
            FIX::TransactTime(UtcTimeStamp()),
            FIX::OrdType(FIX::OrdType_LIMIT)
         );
         msg.set(FIX::Symbol("LKOH"));
         msg.set(FIX::Price(1000.123));
         msg.set(FIX::OrderQty(10000));
         FIX::Session::sendToTarget(msg, FIX::SenderCompID("client"), FIX::TargetCompID("server"));
         usleep(1000);
      }
      initiator.stop();
      return 0;
   }
   catch(FIX::ConfigError& e)
   {
      std::cout << "ERROR: " << e.what() << std::endl;
      return 1;
   }
   return 0;
}
