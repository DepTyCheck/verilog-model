-- Seed: 4017594896494208636,16715549879197889543

library ieee;
use ieee.std_logic_1164.all;

entity smsc is
  port (qaafg : out severity_level; pani : inout real; olrjo : out integer_vector(3 to 3); vjvq : out std_logic_vector(1 to 4));
end smsc;



architecture sp of smsc is
  
begin
  
end sp;



entity fl is
  port (dynrzf : in integer_vector(2 to 2); bghxy : linkage real_vector(4 downto 4); l : buffer time_vector(4 downto 3));
end fl;

library ieee;
use ieee.std_logic_1164.all;

architecture trg of fl is
  signal kwlargyco : integer_vector(3 to 3);
  signal bcozep : real;
  signal ycwxtyxv : severity_level;
  signal ynqxrlbdqb : integer_vector(3 to 3);
  signal uelmw : real;
  signal gkxqyno : severity_level;
  signal sp : std_logic_vector(1 to 4);
  signal nbc : integer_vector(3 to 3);
  signal sokbr : real;
  signal ovxzzyq : severity_level;
begin
  o : entity work.smsc
    port map (qaafg => ovxzzyq, pani => sokbr, olrjo => nbc, vjvq => sp);
  nuj : entity work.smsc
    port map (qaafg => gkxqyno, pani => uelmw, olrjo => ynqxrlbdqb, vjvq => sp);
  jmkrt : entity work.smsc
    port map (qaafg => ycwxtyxv, pani => bcozep, olrjo => kwlargyco, vjvq => sp);
end trg;

library ieee;
use ieee.std_logic_1164.all;

entity efqudax is
  port (cdngewpre : linkage real; ogjtasiorl : buffer bit_vector(1 to 1); ojqabemxp : linkage integer; cyomrhul : inout std_logic_vector(1 to 1));
end efqudax;

library ieee;
use ieee.std_logic_1164.all;

architecture qqozaxsnfv of efqudax is
  signal oeyvpxd : integer_vector(3 to 3);
  signal bsekiw : real;
  signal i : severity_level;
  signal l : std_logic_vector(1 to 4);
  signal vlxtdun : integer_vector(3 to 3);
  signal bymbqzco : real;
  signal lqqejcuhk : severity_level;
  signal me : std_logic_vector(1 to 4);
  signal h : integer_vector(3 to 3);
  signal dzvn : real;
  signal v : severity_level;
begin
  rvltp : entity work.smsc
    port map (qaafg => v, pani => dzvn, olrjo => h, vjvq => me);
  kftqlvswc : entity work.smsc
    port map (qaafg => lqqejcuhk, pani => bymbqzco, olrjo => vlxtdun, vjvq => l);
  mwpref : entity work.smsc
    port map (qaafg => i, pani => bsekiw, olrjo => oeyvpxd, vjvq => me);
end qqozaxsnfv;

library ieee;
use ieee.std_logic_1164.all;

entity rzth is
  port (jjtg : inout integer; gt : buffer std_logic; ao : linkage real);
end rzth;

library ieee;
use ieee.std_logic_1164.all;

architecture lnsbz of rzth is
  signal aqzstun : std_logic_vector(1 to 4);
  signal e : integer_vector(3 to 3);
  signal jpihfaxvet : severity_level;
  signal alrnvqf : std_logic_vector(1 to 1);
  signal a : integer;
  signal dlpzvunop : bit_vector(1 to 1);
  signal qhwbx : real;
begin
  jflhb : entity work.efqudax
    port map (cdngewpre => qhwbx, ogjtasiorl => dlpzvunop, ojqabemxp => a, cyomrhul => alrnvqf);
  iov : entity work.smsc
    port map (qaafg => jpihfaxvet, pani => qhwbx, olrjo => e, vjvq => aqzstun);
end lnsbz;



-- Seed after: 4423670058330184917,16715549879197889543
