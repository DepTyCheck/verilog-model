-- Seed: 17586050985909822736,8118127366649987907

library ieee;
use ieee.std_logic_1164.all;

entity bhkoxb is
  port (zyi : inout real; ac : in std_logic_vector(2 downto 1); ojocyrl : out real);
end bhkoxb;

architecture mpuuoi of bhkoxb is
  
begin
  -- Single-driven assignments
  ojocyrl <= 0.403;
  zyi <= 023.0_4_2;
end mpuuoi;

library ieee;
use ieee.std_logic_1164.all;

entity ocnwfhyw is
  port (grq : out std_logic_vector(0 to 2));
end ocnwfhyw;

library ieee;
use ieee.std_logic_1164.all;

architecture wjr of ocnwfhyw is
  signal ysbl : real;
  signal dxmcf : real;
  signal pf : real;
  signal arwxrmze : std_logic_vector(2 downto 1);
  signal lfge : real;
begin
  o : entity work.bhkoxb
    port map (zyi => lfge, ac => arwxrmze, ojocyrl => pf);
  rtpemhw : entity work.bhkoxb
    port map (zyi => dxmcf, ac => arwxrmze, ojocyrl => ysbl);
end wjr;

entity bcsr is
  port (kmgaep : inout severity_level);
end bcsr;

library ieee;
use ieee.std_logic_1164.all;

architecture yvmdr of bcsr is
  signal s : real;
  signal qqqac : std_logic_vector(2 downto 1);
  signal eeyykms : real;
  signal wnnbbiaedl : std_logic_vector(0 to 2);
  signal f : real;
  signal jhnixtjt : std_logic_vector(2 downto 1);
  signal xwbztbzf : real;
  signal fufywnry : real;
  signal hyqr : std_logic_vector(2 downto 1);
  signal eqcfvqqdu : real;
begin
  cypaj : entity work.bhkoxb
    port map (zyi => eqcfvqqdu, ac => hyqr, ojocyrl => fufywnry);
  bhxn : entity work.bhkoxb
    port map (zyi => xwbztbzf, ac => jhnixtjt, ojocyrl => f);
  k : entity work.ocnwfhyw
    port map (grq => wnnbbiaedl);
  nmko : entity work.bhkoxb
    port map (zyi => eeyykms, ac => qqqac, ojocyrl => s);
  
  -- Multi-driven assignments
  hyqr <= "-Z";
end yvmdr;



-- Seed after: 9928076854786780126,8118127366649987907
