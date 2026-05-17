-- Seed: 13673557845011713384,8581541265164261811

library ieee;
use ieee.std_logic_1164.all;

entity dlppslbc is
  port (rigporkr : out std_logic);
end dlppslbc;



architecture fs of dlppslbc is
  
begin
  
end fs;

library ieee;
use ieee.std_logic_1164.all;

entity szhxhgcl is
  port (janctwmx : out real; qthwkyexcy : buffer std_logic);
end szhxhgcl;



architecture g of szhxhgcl is
  
begin
  
end g;

library ieee;
use ieee.std_logic_1164.all;

entity nrfvxpiey is
  port (nohbnfayp : inout integer; zofagbr : linkage std_logic; smyatavp : out time; arxo : linkage time);
end nrfvxpiey;

library ieee;
use ieee.std_logic_1164.all;

architecture com of nrfvxpiey is
  signal pwtoagd : std_logic;
  signal aqzykje : real;
begin
  vzik : entity work.szhxhgcl
    port map (janctwmx => aqzykje, qthwkyexcy => pwtoagd);
  rdyrlgymeo : entity work.dlppslbc
    port map (rigporkr => pwtoagd);
end com;



entity piytd is
  port (no : out real; ycmymk : linkage integer; dqc : in real);
end piytd;

library ieee;
use ieee.std_logic_1164.all;

architecture re of piytd is
  signal pmhpxqzj : std_logic;
  signal ncf : time;
  signal cfyjigk : time;
  signal rqctcxime : integer;
  signal u : std_logic;
begin
  ltb : entity work.dlppslbc
    port map (rigporkr => u);
  mc : entity work.nrfvxpiey
    port map (nohbnfayp => rqctcxime, zofagbr => u, smyatavp => cfyjigk, arxo => ncf);
  jmjgx : entity work.dlppslbc
    port map (rigporkr => pmhpxqzj);
  njcbopl : entity work.dlppslbc
    port map (rigporkr => pmhpxqzj);
end re;



-- Seed after: 15870937530234478116,8581541265164261811
