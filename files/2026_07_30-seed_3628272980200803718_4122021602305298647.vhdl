-- Seed: 3628272980200803718,4122021602305298647

library ieee;
use ieee.std_logic_1164.all;

entity ewcjq is
  port (nhcpggyblc : in std_logic_vector(2 downto 4); ccepao : out std_logic_vector(1 to 3); yeydwscrz : in std_logic_vector(0 downto 2));
end ewcjq;

architecture aaqpuswty of ewcjq is
  
begin
  
end aaqpuswty;

library ieee;
use ieee.std_logic_1164.all;

entity sdstqglmvt is
  port (gtokx : linkage std_logic; hdta : out bit_vector(4 downto 1));
end sdstqglmvt;

library ieee;
use ieee.std_logic_1164.all;

architecture smb of sdstqglmvt is
  signal dsyvmnso : std_logic_vector(1 to 3);
  signal ooidbtz : std_logic_vector(2 downto 4);
  signal gakoveyo : std_logic_vector(0 downto 2);
  signal hqsr : std_logic_vector(1 to 3);
  signal lfi : std_logic_vector(2 downto 4);
begin
  troavx : entity work.ewcjq
    port map (nhcpggyblc => lfi, ccepao => hqsr, yeydwscrz => gakoveyo);
  kxzktwrupi : entity work.ewcjq
    port map (nhcpggyblc => ooidbtz, ccepao => dsyvmnso, yeydwscrz => gakoveyo);
  
  -- Single-driven assignments
  hdta <= hdta;
  
  -- Multi-driven assignments
  gakoveyo <= (others => '0');
  lfi <= "";
end smb;

entity yemljzgu is
  port (fopwf : linkage real; gqaju : in bit_vector(0 downto 0); olvykcf : inout real; vpzophi : buffer real);
end yemljzgu;

library ieee;
use ieee.std_logic_1164.all;

architecture edftfhg of yemljzgu is
  signal jyonpyhqyd : std_logic_vector(0 downto 2);
  signal zm : std_logic_vector(1 to 3);
  signal tkdmtykvd : std_logic_vector(2 downto 4);
  signal j : std_logic_vector(1 to 3);
  signal ixc : std_logic_vector(0 downto 2);
  signal zqhjdr : std_logic_vector(0 downto 2);
  signal vfn : std_logic_vector(2 downto 4);
  signal hwspuoyek : std_logic_vector(1 to 3);
  signal ctjnjltn : std_logic_vector(0 downto 2);
begin
  z : entity work.ewcjq
    port map (nhcpggyblc => ctjnjltn, ccepao => hwspuoyek, yeydwscrz => ctjnjltn);
  dddvshmsa : entity work.ewcjq
    port map (nhcpggyblc => vfn, ccepao => hwspuoyek, yeydwscrz => zqhjdr);
  alkt : entity work.ewcjq
    port map (nhcpggyblc => ixc, ccepao => j, yeydwscrz => ixc);
  r : entity work.ewcjq
    port map (nhcpggyblc => tkdmtykvd, ccepao => zm, yeydwscrz => jyonpyhqyd);
  
  -- Single-driven assignments
  olvykcf <= 2#01000.001#;
  vpzophi <= 8#607.3#;
  
  -- Multi-driven assignments
  zqhjdr <= (others => '0');
  jyonpyhqyd <= "";
  zm <= hwspuoyek;
end edftfhg;



-- Seed after: 8379371054974668499,4122021602305298647
