-- Seed: 10971144718366722143,12011142928354116943

entity lhdbhuvnnc is
  port (vicvwyvf : linkage time);
end lhdbhuvnnc;

architecture eysnownsg of lhdbhuvnnc is
  
begin
  
end eysnownsg;

library ieee;
use ieee.std_logic_1164.all;

entity wqfmqfgpnz is
  port (jcpyl : linkage std_logic_vector(1 downto 4); gjekrrzf : out boolean);
end wqfmqfgpnz;

architecture tpmu of wqfmqfgpnz is
  signal qgealnbekm : time;
  signal bauhiedhw : time;
begin
  znlhwej : entity work.lhdbhuvnnc
    port map (vicvwyvf => bauhiedhw);
  irtdgpjxn : entity work.lhdbhuvnnc
    port map (vicvwyvf => qgealnbekm);
  
  -- Single-driven assignments
  gjekrrzf <= TRUE;
end tpmu;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (hytf : out time; gskizl : inout std_logic; gdwvxctw : out time_vector(1 downto 0); oe : inout std_logic_vector(3 to 3));
end x;

library ieee;
use ieee.std_logic_1164.all;

architecture uddfvou of x is
  signal ggboka : boolean;
  signal n : std_logic_vector(1 downto 4);
begin
  jgqtjt : entity work.wqfmqfgpnz
    port map (jcpyl => n, gjekrrzf => ggboka);
  
  -- Single-driven assignments
  hytf <= 16#4_7# ps;
  
  -- Multi-driven assignments
  oe <= (others => 'H');
  gskizl <= 'L';
  oe <= "H";
  n <= "";
end uddfvou;

entity dagsjk is
  port (s : buffer real);
end dagsjk;

architecture hfnppdrjl of dagsjk is
  signal cqukqeoih : time;
begin
  vnzsogzxb : entity work.lhdbhuvnnc
    port map (vicvwyvf => cqukqeoih);
  
  -- Single-driven assignments
  s <= 2.0_2;
end hfnppdrjl;



-- Seed after: 10996797118038145436,12011142928354116943
