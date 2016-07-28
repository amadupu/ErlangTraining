



-define(CATEGORY_NONE, 0).
-define(CATEGORY_INTERN, 1).
-define(CATEGORY_EMPLOYEE, 2).
-define(CATEGORY_MANAGER, 3).
-define(CATEGORY_LEAD, 4).


-define(EXPERTISE_NONE, 0).
-define(EXPERTISE_BEGIN,1).
-define(EXPERTISE_MEDIUM, 2).
-define(EXPERTISE_EXPERT, 3).

-record(user,{id=undefined,name=undefined,category=undefined}).
-record(expertise,{id=undefined,expertise=undefined}).
