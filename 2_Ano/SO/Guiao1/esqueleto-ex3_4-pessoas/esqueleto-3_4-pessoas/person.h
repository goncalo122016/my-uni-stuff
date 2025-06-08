typedef struct person {
    char name[200];
    int age;
} Person;

int insertPerson(char* name, int age);

int listPersons(int N);

int changeAge(char* name, int age);
    