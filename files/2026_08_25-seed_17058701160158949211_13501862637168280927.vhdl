-- Seed: 17058701160158949211,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity ifkjymcwm is
  port (dsqyrnxaia : buffer integer_vector(1 to 4); htkvoadt : in real; dpesc : out integer_vector(2 to 2); kfbqmpli : out std_logic);
end ifkjymcwm;

architecture wrhb of ifkjymcwm is
  
begin
  -- Single-driven assignments
  dsqyrnxaia <= dsqyrnxaia;
  dpesc <= (others => 16#6#);
  
  -- Multi-driven assignments
  kfbqmpli <= kfbqmpli;
  kfbqmpli <= kfbqmpli;
  kfbqmpli <= 'Z';
  kfbqmpli <= '0';
end wrhb;

entity hn is
  port (jlltfs : out integer; abtzmqk : out bit_vector(0 downto 4));
end hn;

architecture sfcxsdxbpu of hn is
  
begin
  -- Single-driven assignments
  abtzmqk <= (others => '0');
  jlltfs <= jlltfs;
end sfcxsdxbpu;

entity admaa is
  port (lzhjmvyvcz : buffer time);
end admaa;

library ieee;
use ieee.std_logic_1164.all;

architecture wahlnxab of admaa is
  signal srbrqu : std_logic;
  signal axtv : integer_vector(2 to 2);
  signal u : integer_vector(1 to 4);
  signal acaqsbjskm : bit_vector(0 downto 4);
  signal hwog : integer;
  signal uwizhjxpvl : std_logic;
  signal nqzvamqml : integer_vector(2 to 2);
  signal vibnccvuvx : real;
  signal xzldjelt : integer_vector(1 to 4);
  signal jsiwle : std_logic;
  signal vai : integer_vector(2 to 2);
  signal bpcswwah : real;
  signal sdar : integer_vector(1 to 4);
begin
  p : entity work.ifkjymcwm
    port map (dsqyrnxaia => sdar, htkvoadt => bpcswwah, dpesc => vai, kfbqmpli => jsiwle);
  zbklix : entity work.ifkjymcwm
    port map (dsqyrnxaia => xzldjelt, htkvoadt => vibnccvuvx, dpesc => nqzvamqml, kfbqmpli => uwizhjxpvl);
  ishllb : entity work.hn
    port map (jlltfs => hwog, abtzmqk => acaqsbjskm);
  qlwcxrw : entity work.ifkjymcwm
    port map (dsqyrnxaia => u, htkvoadt => bpcswwah, dpesc => axtv, kfbqmpli => srbrqu);
  
  -- Single-driven assignments
  bpcswwah <= 412.2102;
  vibnccvuvx <= bpcswwah;
  lzhjmvyvcz <= 2#1_1_0_0_1# fs;
  
  -- Multi-driven assignments
  jsiwle <= jsiwle;
  jsiwle <= jsiwle;
  jsiwle <= 'Z';
end wahlnxab;



-- Seed after: 17826015047787682043,13501862637168280927
