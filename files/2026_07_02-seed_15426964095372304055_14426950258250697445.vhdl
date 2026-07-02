-- Seed: 15426964095372304055,14426950258250697445

library ieee;
use ieee.std_logic_1164.all;

entity gblc is
  port (mdhyabgop : inout time_vector(2 downto 2); anffs : in boolean; tiaqbxh : linkage string(1 to 3); fekl : in std_logic);
end gblc;

architecture kievscbm of gblc is
  
begin
  -- Single-driven assignments
  mdhyabgop <= (others => 16#C.8_4# ms);
end kievscbm;

library ieee;
use ieee.std_logic_1164.all;

entity dxmvl is
  port (wpuwuupb : out integer; jttg : in std_logic_vector(2 downto 0); xse : in std_logic_vector(1 to 1));
end dxmvl;

library ieee;
use ieee.std_logic_1164.all;

architecture bptg of dxmvl is
  signal qitec : std_logic;
  signal a : string(1 to 3);
  signal pij : time_vector(2 downto 2);
  signal lo : string(1 to 3);
  signal nprdakgai : time_vector(2 downto 2);
  signal nkq : std_logic;
  signal lmob : string(1 to 3);
  signal agxaxzkihc : boolean;
  signal s : time_vector(2 downto 2);
  signal xjonehehr : std_logic;
  signal cocpnqit : string(1 to 3);
  signal zepfobuo : boolean;
  signal asleikeq : time_vector(2 downto 2);
begin
  t : entity work.gblc
    port map (mdhyabgop => asleikeq, anffs => zepfobuo, tiaqbxh => cocpnqit, fekl => xjonehehr);
  zrv : entity work.gblc
    port map (mdhyabgop => s, anffs => agxaxzkihc, tiaqbxh => lmob, fekl => nkq);
  w : entity work.gblc
    port map (mdhyabgop => nprdakgai, anffs => zepfobuo, tiaqbxh => lo, fekl => xjonehehr);
  gf : entity work.gblc
    port map (mdhyabgop => pij, anffs => zepfobuo, tiaqbxh => a, fekl => qitec);
  
  -- Single-driven assignments
  wpuwuupb <= wpuwuupb;
  agxaxzkihc <= zepfobuo;
  
  -- Multi-driven assignments
  xjonehehr <= xjonehehr;
  xjonehehr <= qitec;
  xjonehehr <= '0';
end bptg;



-- Seed after: 5638513095234151987,14426950258250697445
