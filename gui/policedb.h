#ifndef POLICEDB_H
#define POLICEDB_H

#include <QMainWindow>

namespace Ui {
class PoliceDB;
}

class PoliceDB : public QMainWindow
{
    Q_OBJECT
    
public:
    explicit PoliceDB(QWidget *parent = 0);
    ~PoliceDB();
    
private:
    Ui::PoliceDB *ui;
};

#endif // POLICEDB_H
