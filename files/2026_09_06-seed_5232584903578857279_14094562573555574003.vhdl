-- Seed: 5232584903578857279,14094562573555574003

library ieee;
use ieee.std_logic_1164.all;

entity sdfqb is
  port (wtchzxnacm : linkage std_logic; odo : linkage time; rwes : out real; egctqppra : in integer);
end sdfqb;

architecture egy of sdfqb is
  
begin
  
end egy;

entity gfncokfy is
  port (cussmjler : buffer real);
end gfncokfy;

library ieee;
use ieee.std_logic_1164.all;

architecture v of gfncokfy is
  signal rdxp : time;
  signal xyczh : real;
  signal yrfuxu : time;
  signal nblhbrs : std_logic;
  signal zvrlaalxr : integer;
  signal zioaeypffl : real;
  signal uhkhnpud : time;
  signal yskez : std_logic;
begin
  o : entity work.sdfqb
    port map (wtchzxnacm => yskez, odo => uhkhnpud, rwes => zioaeypffl, egctqppra => zvrlaalxr);
  mhdrbtxuus : entity work.sdfqb
    port map (wtchzxnacm => nblhbrs, odo => yrfuxu, rwes => xyczh, egctqppra => zvrlaalxr);
  luyy : entity work.sdfqb
    port map (wtchzxnacm => nblhbrs, odo => rdxp, rwes => cussmjler, egctqppra => zvrlaalxr);
end v;

entity jceojh is
  port (rwguphyrlp : linkage integer; qmibltggz : in integer; e : in time; zj : in integer);
end jceojh;

library ieee;
use ieee.std_logic_1164.all;

architecture o of jceojh is
  signal okqbadxrkh : integer;
  signal h : real;
  signal cebsidbwhk : time;
  signal wnuwzila : std_logic;
begin
  tiyi : entity work.sdfqb
    port map (wtchzxnacm => wnuwzila, odo => cebsidbwhk, rwes => h, egctqppra => okqbadxrkh);
  
  -- Single-driven assignments
  okqbadxrkh <= zj;
end o;



-- Seed after: 426863612100879083,14094562573555574003
