-- Seed: 11820214300743230484,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity aqdnsed is
  port (ozjciqouju : out time; x : out std_logic_vector(2 downto 4));
end aqdnsed;

architecture bwyb of aqdnsed is
  
begin
  -- Single-driven assignments
  ozjciqouju <= 422 ns;
  
  -- Multi-driven assignments
  x <= "";
end bwyb;

library ieee;
use ieee.std_logic_1164.all;

entity lyt is
  port (mdi : inout std_logic_vector(0 to 2); oz : linkage std_logic_vector(1 downto 0); ahmxn : linkage character);
end lyt;

library ieee;
use ieee.std_logic_1164.all;

architecture sweuejogk of lyt is
  signal e : time;
  signal vzobbt : time;
  signal czuxn : std_logic_vector(2 downto 4);
  signal w : time;
begin
  djxyc : entity work.aqdnsed
    port map (ozjciqouju => w, x => czuxn);
  vctbnlukr : entity work.aqdnsed
    port map (ozjciqouju => vzobbt, x => czuxn);
  rab : entity work.aqdnsed
    port map (ozjciqouju => e, x => czuxn);
end sweuejogk;

library ieee;
use ieee.std_logic_1164.all;

entity dm is
  port (mqncw : inout std_logic_vector(2 to 2); a : in integer; g : linkage integer);
end dm;

library ieee;
use ieee.std_logic_1164.all;

architecture yufh of dm is
  signal etscahfk : std_logic_vector(2 downto 4);
  signal fl : time;
  signal dowyzp : character;
  signal xfvapp : std_logic_vector(1 downto 0);
  signal wuwkzlx : std_logic_vector(0 to 2);
  signal oeojdxavbo : std_logic_vector(2 downto 4);
  signal wjh : time;
begin
  adqbgnfxvq : entity work.aqdnsed
    port map (ozjciqouju => wjh, x => oeojdxavbo);
  zzcwaij : entity work.lyt
    port map (mdi => wuwkzlx, oz => xfvapp, ahmxn => dowyzp);
  blfr : entity work.aqdnsed
    port map (ozjciqouju => fl, x => etscahfk);
  
  -- Multi-driven assignments
  wuwkzlx <= ('Z', 'Z', 'X');
  mqncw <= (others => 'Z');
  etscahfk <= "";
end yufh;

entity spvnrowwq is
  port (txsgzr : in integer; jifslvk : out time_vector(0 to 2); euyvvzase : linkage time);
end spvnrowwq;

library ieee;
use ieee.std_logic_1164.all;

architecture pda of spvnrowwq is
  signal phugtqjtmu : character;
  signal zblv : std_logic_vector(1 downto 0);
  signal dmgcsta : std_logic_vector(0 to 2);
begin
  incbwk : entity work.lyt
    port map (mdi => dmgcsta, oz => zblv, ahmxn => phugtqjtmu);
  
  -- Single-driven assignments
  jifslvk <= (3_2 us, 2#0011# fs, 8#3_4_7_3.0453# fs);
  
  -- Multi-driven assignments
  dmgcsta <= ('0', 'Z', '1');
end pda;



-- Seed after: 1466548276758615579,14652815260262078753
