-- Seed: 5992819735324850216,8067602802092121131

library ieee;
use ieee.std_logic_1164.all;

entity l is
  port (irdvmpwk : inout std_logic; hdohlw : out real; d : in integer_vector(2 to 3));
end l;

architecture qvnimp of l is
  
begin
  -- Single-driven assignments
  hdohlw <= 2.22;
end qvnimp;

library ieee;
use ieee.std_logic_1164.all;

entity slydi is
  port (ktcuwil : in std_logic_vector(0 to 1); uwehtzjy : out integer; sgonhnbs : in time; mqtd : buffer time_vector(3 downto 1));
end slydi;

library ieee;
use ieee.std_logic_1164.all;

architecture acsxswstc of slydi is
  signal luqf : integer_vector(2 to 3);
  signal lvcrgpl : real;
  signal bqbwb : std_logic;
  signal vb : integer_vector(2 to 3);
  signal uquywyaywr : real;
  signal rh : integer_vector(2 to 3);
  signal ziwxqqxhex : real;
  signal ezy : std_logic;
begin
  dfda : entity work.l
    port map (irdvmpwk => ezy, hdohlw => ziwxqqxhex, d => rh);
  zt : entity work.l
    port map (irdvmpwk => ezy, hdohlw => uquywyaywr, d => vb);
  kxpb : entity work.l
    port map (irdvmpwk => bqbwb, hdohlw => lvcrgpl, d => luqf);
  
  -- Single-driven assignments
  mqtd <= (2#01.0# fs, 8#7724# ms, 3 sec);
  vb <= (16#9_4_2#, 8#3#);
  rh <= rh;
  uwehtzjy <= 3;
end acsxswstc;

library ieee;
use ieee.std_logic_1164.all;

entity pzt is
  port (z : linkage time; zuq : buffer std_logic_vector(1 to 1));
end pzt;

library ieee;
use ieee.std_logic_1164.all;

architecture u of pzt is
  signal ljqzkd : integer_vector(2 to 3);
  signal inajshgf : real;
  signal rosak : std_logic;
  signal dee : integer_vector(2 to 3);
  signal tpd : real;
  signal t : std_logic;
  signal bv : integer_vector(2 to 3);
  signal xjbwhixlpg : real;
  signal uxoynnhyl : std_logic;
begin
  rdiortp : entity work.l
    port map (irdvmpwk => uxoynnhyl, hdohlw => xjbwhixlpg, d => bv);
  edtnh : entity work.l
    port map (irdvmpwk => t, hdohlw => tpd, d => dee);
  gy : entity work.l
    port map (irdvmpwk => rosak, hdohlw => inajshgf, d => ljqzkd);
  
  -- Single-driven assignments
  dee <= bv;
  ljqzkd <= bv;
  bv <= (3, 16#2CE9D#);
  
  -- Multi-driven assignments
  uxoynnhyl <= 'X';
  t <= 'H';
  zuq <= zuq;
end u;



-- Seed after: 12975387160120949860,8067602802092121131
