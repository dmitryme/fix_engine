#include <quickfix/FileStore.h>
#include <quickfix/FileLog.h>
#include <quickfix/SocketAcceptor.h>
#include <quickfix/Session.h>
#include <quickfix/SessionSettings.h>
#include <quickfix/Application.h>
#include <quickfix/fix44/ExecutionReport.h>

#include  <stdio.h>

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
      virtual void toApp(Message&, const SessionID&) throw(DoNotSend)
      {
      }
      virtual void fromAdmin(const Message&, const SessionID&) throw( FieldNotFound, IncorrectDataFormat, IncorrectTagValue, RejectLogon)
      {

      }
      virtual void fromApp(const Message& msg, const SessionID&) throw( FieldNotFound, IncorrectDataFormat, IncorrectTagValue, UnsupportedMessageType)
      {
         FIX::MsgType type;
         msg.getHeader().getField(type);
         if (type.getValue() == "D")
         {
            static int orderID = 0;
            std::ostringstream ost;
            ost << "OID_" << ++orderID;
            FIX44::ExecutionReport er(
                  FIX::OrderID(ost.str()),
                  FIX::ExecID("123"),
                  FIX::ExecType(FIX::ExecType_NEW),
                  FIX::OrdStatus(FIX::OrdStatus_NEW),
                  FIX::Side(FIX::Side_BUY),
                  FIX::LeavesQty(100),
                  FIX::CumQty(0),
                  FIX::AvgPx(0.0)
            );
            FIX::ClOrdID clOrdID;
            msg.getField(clOrdID);
            er.set(FIX::ClOrdID(clOrdID));
            FIX::Session::sendToTarget(er, FIX::SenderCompID("server"), FIX::TargetCompID("client"));
         }
      }
};

int main(int argc, char *argv[])
{
   try
   {
      FIX::SessionSettings settings("qfix_server.cfg");

      MyApplication application;
      FIX::FileStoreFactory storeFactory(settings);
      FIX::FileLogFactory logFactory(settings);
      FIX::SocketAcceptor acceptor(application, storeFactory, settings, logFactory);
      acceptor.start();
      getchar();
      acceptor.stop();
      return 0;
   }
   catch(FIX::ConfigError& e)
   {
      std::cout << "ERROR: " << e.what() << std::endl;
      return 1;
   }
   return 0;
}
