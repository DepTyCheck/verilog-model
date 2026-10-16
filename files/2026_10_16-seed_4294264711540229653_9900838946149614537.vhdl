-- Seed: 4294264711540229653,9900838946149614537

library ieee;
use ieee.std_logic_1164.all;

entity s is
  port (x : inout time_vector(4 to 2); gqeollpq : linkage std_logic; upqekylj : buffer boolean; hts : buffer time);
end s;

architecture gpjg of s is
  
begin
  -- Single-driven assignments
  hts <= 2#0_0_1_0_0.1# ns;
  x <= x;
  upqekylj <= upqekylj;
end gpjg;

entity ixpizqpjuh is
  port (zs : linkage time);
end ixpizqpjuh;

library ieee;
use ieee.std_logic_1164.all;

architecture bx of ixpizqpjuh is
  signal lujmzfbver : time;
  signal azrgztvp : boolean;
  signal fcyq : time_vector(4 to 2);
  signal qgq : time;
  signal zlogbndtz : boolean;
  signal ysc : time_vector(4 to 2);
  signal qn : time;
  signal pu : boolean;
  signal hehqzl : time_vector(4 to 2);
  signal fjbbxi : time;
  signal gemd : boolean;
  signal vh : std_logic;
  signal rt : time_vector(4 to 2);
begin
  krrccsh : entity work.s
    port map (x => rt, gqeollpq => vh, upqekylj => gemd, hts => fjbbxi);
  flbuvyq : entity work.s
    port map (x => hehqzl, gqeollpq => vh, upqekylj => pu, hts => qn);
  expoiigq : entity work.s
    port map (x => ysc, gqeollpq => vh, upqekylj => zlogbndtz, hts => qgq);
  jzqt : entity work.s
    port map (x => fcyq, gqeollpq => vh, upqekylj => azrgztvp, hts => lujmzfbver);
end bx;

entity ev is
  port (fi : in integer);
end ev;

library ieee;
use ieee.std_logic_1164.all;

architecture orsx of ev is
  signal dnb : time;
  signal rzrtungma : time;
  signal vpkhc : boolean;
  signal iurhahi : std_logic;
  signal englrou : time_vector(4 to 2);
  signal oczveytzw : time;
  signal qakphyw : boolean;
  signal rqrdldfauj : std_logic;
  signal yltdyfzjvh : time_vector(4 to 2);
  signal rjhfcjl : time;
begin
  xv : entity work.ixpizqpjuh
    port map (zs => rjhfcjl);
  iag : entity work.s
    port map (x => yltdyfzjvh, gqeollpq => rqrdldfauj, upqekylj => qakphyw, hts => oczveytzw);
  bvzykvri : entity work.s
    port map (x => englrou, gqeollpq => iurhahi, upqekylj => vpkhc, hts => rzrtungma);
  zy : entity work.ixpizqpjuh
    port map (zs => dnb);
  
  -- Multi-driven assignments
  rqrdldfauj <= 'Z';
end orsx;

entity zjzujkk is
  port (vdlsc : buffer severity_level; abgkymoc : buffer boolean);
end zjzujkk;

library ieee;
use ieee.std_logic_1164.all;

architecture g of zjzujkk is
  signal joeemqljm : integer;
  signal fjtk : time;
  signal xsqhuvae : boolean;
  signal vjgl : std_logic;
  signal rqnkyptqql : time_vector(4 to 2);
begin
  yvzswurw : entity work.s
    port map (x => rqnkyptqql, gqeollpq => vjgl, upqekylj => xsqhuvae, hts => fjtk);
  ikk : entity work.ev
    port map (fi => joeemqljm);
  mkcmkkz : entity work.ev
    port map (fi => joeemqljm);
  
  -- Single-driven assignments
  abgkymoc <= abgkymoc;
  joeemqljm <= 3;
  vdlsc <= vdlsc;
end g;



-- Seed after: 10416581932592197095,9900838946149614537
