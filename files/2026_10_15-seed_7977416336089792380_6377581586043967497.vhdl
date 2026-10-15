-- Seed: 7977416336089792380,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity lhak is
  port (glpyhkti : in std_logic_vector(0 to 1); oq : linkage std_logic_vector(4 to 4); yy : in integer; hw : buffer bit_vector(0 downto 2));
end lhak;

architecture ou of lhak is
  
begin
  -- Single-driven assignments
  hw <= hw;
end ou;

library ieee;
use ieee.std_logic_1164.all;

entity dnfswp is
  port (x : out std_logic; ecfkl : linkage integer; hubixcu : inout std_logic; crphniqjg : inout std_logic);
end dnfswp;

library ieee;
use ieee.std_logic_1164.all;

architecture gkbbkm of dnfswp is
  signal o : bit_vector(0 downto 2);
  signal jdhokjjprx : std_logic_vector(4 to 4);
  signal ruunhwyzw : bit_vector(0 downto 2);
  signal juki : integer;
  signal yzk : std_logic_vector(4 to 4);
  signal tvbshcgvm : bit_vector(0 downto 2);
  signal kr : integer;
  signal dmwmfsu : std_logic_vector(4 to 4);
  signal wlbwhbzrz : std_logic_vector(0 to 1);
begin
  nng : entity work.lhak
    port map (glpyhkti => wlbwhbzrz, oq => dmwmfsu, yy => kr, hw => tvbshcgvm);
  ytfrndvo : entity work.lhak
    port map (glpyhkti => wlbwhbzrz, oq => yzk, yy => juki, hw => ruunhwyzw);
  xcgtmqtu : entity work.lhak
    port map (glpyhkti => wlbwhbzrz, oq => jdhokjjprx, yy => kr, hw => o);
  
  -- Multi-driven assignments
  yzk <= (others => '-');
end gkbbkm;



-- Seed after: 368346266512361220,6377581586043967497
