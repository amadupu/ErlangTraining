
{
  application, imserver,
  [
     {description, "IM Server"},
     {vsn, "2"},
     {modules, [client, controller, dbservice, servicemgr, imserver_app]},
     {registered, [controller,dbservice,servicemgr]},
     {applications, [kernel, stdlib, sasl]},
     {mod, {imserver_app, []}},
     {env, [{port,1234}]}
  ]
}.
