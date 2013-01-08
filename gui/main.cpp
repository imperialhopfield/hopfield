#include "policedb.h"
#include <QApplication>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    PoliceDB w;
    w.show();
    
    return a.exec();
}
