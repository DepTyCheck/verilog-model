-- Seed: 9354634893050213872,662889661651915549

library ieee;
use ieee.std_logic_1164.all;

entity jetjacsh is
  port (ydnws : in std_logic_vector(3 to 0); iz : linkage std_logic; kr : in time; n : inout bit_vector(4 downto 0));
end jetjacsh;

architecture edqdz of jetjacsh is
  
begin
  -- Single-driven assignments
  n <= n;
end edqdz;

library ieee;
use ieee.std_logic_1164.all;

entity iy is
  port (cpbfusyjmi : out real; lafjot : inout std_logic; cydyruq : linkage severity_level; obwjcw : inout integer_vector(4 to 4));
end iy;

library ieee;
use ieee.std_logic_1164.all;

architecture o of iy is
  signal ndhi : bit_vector(4 downto 0);
  signal mzg : time;
  signal gfem : bit_vector(4 downto 0);
  signal ttwbrjkly : std_logic;
  signal ah : bit_vector(4 downto 0);
  signal tyi : time;
  signal jhazowlkea : std_logic_vector(3 to 0);
begin
  gb : entity work.jetjacsh
    port map (ydnws => jhazowlkea, iz => lafjot, kr => tyi, n => ah);
  lrkpdh : entity work.jetjacsh
    port map (ydnws => jhazowlkea, iz => ttwbrjkly, kr => tyi, n => gfem);
  wrf : entity work.jetjacsh
    port map (ydnws => jhazowlkea, iz => lafjot, kr => mzg, n => ndhi);
  
  -- Single-driven assignments
  obwjcw <= (others => 01);
  mzg <= tyi;
  tyi <= tyi;
  cpbfusyjmi <= cpbfusyjmi;
  
  -- Multi-driven assignments
  jhazowlkea <= (others => '0');
end o;

library ieee;
use ieee.std_logic_1164.all;

entity zuwbicme is
  port (cwdrdwob : linkage std_logic; jolmo : inout std_logic_vector(1 to 1));
end zuwbicme;

library ieee;
use ieee.std_logic_1164.all;

architecture glzu of zuwbicme is
  signal pv : bit_vector(4 downto 0);
  signal zoaycth : time;
  signal tlaouqpazv : std_logic;
  signal sgkjdm : bit_vector(4 downto 0);
  signal kpkehr : bit_vector(4 downto 0);
  signal lxjeq : time;
  signal uhczjajzbl : std_logic;
  signal dmhrw : std_logic_vector(3 to 0);
begin
  hynfysu : entity work.jetjacsh
    port map (ydnws => dmhrw, iz => uhczjajzbl, kr => lxjeq, n => kpkehr);
  iujktbogoy : entity work.jetjacsh
    port map (ydnws => dmhrw, iz => cwdrdwob, kr => lxjeq, n => sgkjdm);
  wevm : entity work.jetjacsh
    port map (ydnws => dmhrw, iz => tlaouqpazv, kr => zoaycth, n => pv);
  
  -- Single-driven assignments
  lxjeq <= 2#0_0# fs;
  zoaycth <= lxjeq;
  
  -- Multi-driven assignments
  tlaouqpazv <= '0';
  dmhrw <= (others => '0');
  jolmo <= "X";
end glzu;



-- Seed after: 16930599630429460982,662889661651915549
