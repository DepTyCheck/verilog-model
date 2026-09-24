-- Seed: 13498816785697722468,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity b is
  port (lvvvcwrt : linkage std_logic_vector(0 downto 4); rltupvkr : linkage time_vector(4 to 3); qnrzo : inout bit_vector(0 downto 4));
end b;

architecture josvauwdvt of b is
  
begin
  -- Single-driven assignments
  qnrzo <= (others => '0');
end josvauwdvt;

entity hlseaz is
  port (yllcwdq : linkage bit; wqcydvf : out time);
end hlseaz;

library ieee;
use ieee.std_logic_1164.all;

architecture fzjekss of hlseaz is
  signal ryrtugp : bit_vector(0 downto 4);
  signal hvvyf : time_vector(4 to 3);
  signal flyrcs : std_logic_vector(0 downto 4);
begin
  qthjz : entity work.b
    port map (lvvvcwrt => flyrcs, rltupvkr => hvvyf, qnrzo => ryrtugp);
  
  -- Single-driven assignments
  wqcydvf <= 8#5# us;
  
  -- Multi-driven assignments
  flyrcs <= flyrcs;
  flyrcs <= (others => '0');
end fzjekss;

library ieee;
use ieee.std_logic_1164.all;

entity band is
  port (hbegpm : linkage integer; qpi : linkage std_logic_vector(2 downto 3); iyovgtic : out std_logic; sis : inout real);
end band;

library ieee;
use ieee.std_logic_1164.all;

architecture bjtfkghlh of band is
  signal wdi : time;
  signal mrv : bit;
  signal sdkuds : bit_vector(0 downto 4);
  signal x : time_vector(4 to 3);
  signal zqci : std_logic_vector(0 downto 4);
  signal zdb : bit_vector(0 downto 4);
  signal gfvivoq : time_vector(4 to 3);
  signal o : std_logic_vector(0 downto 4);
begin
  ykutefsh : entity work.b
    port map (lvvvcwrt => o, rltupvkr => gfvivoq, qnrzo => zdb);
  zstoewdc : entity work.b
    port map (lvvvcwrt => zqci, rltupvkr => x, qnrzo => sdkuds);
  vmv : entity work.hlseaz
    port map (yllcwdq => mrv, wqcydvf => wdi);
  
  -- Single-driven assignments
  sis <= 8#4_7.1_2_6#;
  
  -- Multi-driven assignments
  zqci <= "";
  iyovgtic <= iyovgtic;
  iyovgtic <= iyovgtic;
  iyovgtic <= 'U';
end bjtfkghlh;



-- Seed after: 10020172575035905,17234720251424330329
