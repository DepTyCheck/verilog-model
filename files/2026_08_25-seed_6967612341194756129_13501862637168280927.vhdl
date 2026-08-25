-- Seed: 6967612341194756129,13501862637168280927

library ieee;
use ieee.std_logic_1164.all;

entity iuuo is
  port (yx : buffer real; get : inout real_vector(1 downto 1); vtxfv : in std_logic_vector(1 to 3));
end iuuo;

architecture gac of iuuo is
  
begin
  -- Single-driven assignments
  get <= (others => 8#4_3_4_5.1#);
  yx <= 3_2_4_1_0.21;
end gac;

entity mgbmyz is
  port (bfbt : out integer; mmwc : linkage time; piqc : in integer; pomsrewgmr : in boolean_vector(3 to 2));
end mgbmyz;

library ieee;
use ieee.std_logic_1164.all;

architecture dzrfeypvp of mgbmyz is
  signal stozk : real_vector(1 downto 1);
  signal qohmdzgj : real;
  signal vpwafis : std_logic_vector(1 to 3);
  signal gdl : real_vector(1 downto 1);
  signal xnjlq : real;
  signal oscgqr : std_logic_vector(1 to 3);
  signal iyulor : real_vector(1 downto 1);
  signal inyqlcvji : real;
  signal ezouicautw : std_logic_vector(1 to 3);
  signal ylf : real_vector(1 downto 1);
  signal mhz : real;
begin
  dhsf : entity work.iuuo
    port map (yx => mhz, get => ylf, vtxfv => ezouicautw);
  pxfu : entity work.iuuo
    port map (yx => inyqlcvji, get => iyulor, vtxfv => oscgqr);
  obm : entity work.iuuo
    port map (yx => xnjlq, get => gdl, vtxfv => vpwafis);
  cqzrguaqy : entity work.iuuo
    port map (yx => qohmdzgj, get => stozk, vtxfv => ezouicautw);
  
  -- Single-driven assignments
  bfbt <= piqc;
end dzrfeypvp;

library ieee;
use ieee.std_logic_1164.all;

entity pg is
  port (t : in std_logic_vector(3 to 1); gxranvv : out real);
end pg;

architecture gxic of pg is
  
begin
  -- Single-driven assignments
  gxranvv <= gxranvv;
end gxic;

library ieee;
use ieee.std_logic_1164.all;

entity ldrnisz is
  port (uvqje : out std_logic_vector(4 downto 0); puqy : in std_logic_vector(0 to 4));
end ldrnisz;

library ieee;
use ieee.std_logic_1164.all;

architecture lfjpy of ldrnisz is
  signal rke : real;
  signal ozciody : std_logic_vector(3 to 1);
  signal zxhfia : std_logic_vector(1 to 3);
  signal btjb : real_vector(1 downto 1);
  signal d : real;
  signal lyjqfu : boolean_vector(3 to 2);
  signal cakedngmo : integer;
  signal i : time;
  signal uichuhxnl : integer;
begin
  voirfsounc : entity work.mgbmyz
    port map (bfbt => uichuhxnl, mmwc => i, piqc => cakedngmo, pomsrewgmr => lyjqfu);
  lbcjgpfx : entity work.iuuo
    port map (yx => d, get => btjb, vtxfv => zxhfia);
  wfjvpreolq : entity work.pg
    port map (t => ozciody, gxranvv => rke);
  
  -- Multi-driven assignments
  uvqje <= puqy;
  ozciody <= (others => '0');
  uvqje <= ('Z', '0', 'Z', 'W', 'W');
  ozciody <= (others => '0');
end lfjpy;



-- Seed after: 2031687203915627633,13501862637168280927
