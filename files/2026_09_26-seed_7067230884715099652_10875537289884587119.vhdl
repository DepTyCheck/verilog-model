-- Seed: 7067230884715099652,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity d is
  port (t : in integer_vector(0 downto 1); zpneh : linkage std_logic_vector(0 downto 4));
end d;

architecture pp of d is
  
begin
  
end pp;

library ieee;
use ieee.std_logic_1164.all;

entity q is
  port (h : buffer time; lekqxq : in real; racmcrzwf : buffer std_logic_vector(4 downto 2));
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture wya of q is
  signal fplr : integer_vector(0 downto 1);
  signal xfjgjcp : integer_vector(0 downto 1);
  signal gjsmkwcvym : std_logic_vector(0 downto 4);
  signal cvoozplxc : integer_vector(0 downto 1);
begin
  isosybfw : entity work.d
    port map (t => cvoozplxc, zpneh => gjsmkwcvym);
  k : entity work.d
    port map (t => xfjgjcp, zpneh => gjsmkwcvym);
  vldadriuvz : entity work.d
    port map (t => fplr, zpneh => gjsmkwcvym);
  
  -- Single-driven assignments
  fplr <= cvoozplxc;
  xfjgjcp <= xfjgjcp;
  cvoozplxc <= (others => 0);
  h <= h;
  
  -- Multi-driven assignments
  gjsmkwcvym <= gjsmkwcvym;
  gjsmkwcvym <= "";
  gjsmkwcvym <= (others => '0');
end wya;

library ieee;
use ieee.std_logic_1164.all;

entity lxrikh is
  port (evoanc : inout std_logic_vector(3 downto 1); pcxqkqjuwy : in real; acc : linkage real; ux : linkage std_logic);
end lxrikh;

library ieee;
use ieee.std_logic_1164.all;

architecture jpkdmwdfc of lxrikh is
  signal czfhbfd : real;
  signal ioexb : time;
  signal emuawllan : time;
  signal hwmld : std_logic_vector(0 downto 4);
  signal ubjgt : integer_vector(0 downto 1);
begin
  dfxkxqgq : entity work.d
    port map (t => ubjgt, zpneh => hwmld);
  pnz : entity work.q
    port map (h => emuawllan, lekqxq => pcxqkqjuwy, racmcrzwf => evoanc);
  agr : entity work.q
    port map (h => ioexb, lekqxq => czfhbfd, racmcrzwf => evoanc);
  
  -- Single-driven assignments
  ubjgt <= (others => 0);
  czfhbfd <= 16#F.0D834#;
  
  -- Multi-driven assignments
  evoanc <= evoanc;
  evoanc <= ('U', 'U', 'X');
  evoanc <= evoanc;
  evoanc <= "XUU";
end jpkdmwdfc;

library ieee;
use ieee.std_logic_1164.all;

entity uu is
  port (vbewh : buffer time; dboywwtst : buffer std_logic);
end uu;

architecture vb of uu is
  
begin
  -- Multi-driven assignments
  dboywwtst <= dboywwtst;
  dboywwtst <= dboywwtst;
  dboywwtst <= dboywwtst;
end vb;



-- Seed after: 3257045583868351943,10875537289884587119
