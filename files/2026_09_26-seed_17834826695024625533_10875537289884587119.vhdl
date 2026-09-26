-- Seed: 17834826695024625533,10875537289884587119

entity csonnj is
  port (bcfvlp : buffer time_vector(1 downto 2));
end csonnj;

architecture gwlaxg of csonnj is
  
begin
  -- Single-driven assignments
  bcfvlp <= (others => 0 ns);
end gwlaxg;

library ieee;
use ieee.std_logic_1164.all;

entity x is
  port (icabo : in bit_vector(3 to 4); wxtsqagop : inout time; uokwtpv : out std_logic_vector(1 to 4); lotpysbn : out integer);
end x;

architecture cyfg of x is
  signal xzzb : time_vector(1 downto 2);
  signal xcnkamcvc : time_vector(1 downto 2);
  signal gqxp : time_vector(1 downto 2);
  signal labmh : time_vector(1 downto 2);
begin
  bvteyilmx : entity work.csonnj
    port map (bcfvlp => labmh);
  jxuzwac : entity work.csonnj
    port map (bcfvlp => gqxp);
  je : entity work.csonnj
    port map (bcfvlp => xcnkamcvc);
  lcg : entity work.csonnj
    port map (bcfvlp => xzzb);
  
  -- Single-driven assignments
  lotpysbn <= 2_0;
  wxtsqagop <= wxtsqagop;
  
  -- Multi-driven assignments
  uokwtpv <= ('Z', 'L', 'U', '-');
  uokwtpv <= "HLXU";
  uokwtpv <= ('W', '0', 'L', 'L');
end cyfg;

library ieee;
use ieee.std_logic_1164.all;

entity vu is
  port (stgstp : in bit; qhgos : out time; ogxe : in std_logic_vector(4 downto 3); bpkifhkz : linkage integer);
end vu;

library ieee;
use ieee.std_logic_1164.all;

architecture ehyicv of vu is
  signal hdoidx : time_vector(1 downto 2);
  signal ifrscle : time_vector(1 downto 2);
  signal hn : integer;
  signal hdaaddir : std_logic_vector(1 to 4);
  signal uwkejzgvne : time;
  signal c : bit_vector(3 to 4);
begin
  lrghj : entity work.x
    port map (icabo => c, wxtsqagop => uwkejzgvne, uokwtpv => hdaaddir, lotpysbn => hn);
  grxxneom : entity work.csonnj
    port map (bcfvlp => ifrscle);
  lrwzc : entity work.csonnj
    port map (bcfvlp => hdoidx);
  
  -- Single-driven assignments
  qhgos <= qhgos;
  
  -- Multi-driven assignments
  hdaaddir <= hdaaddir;
  hdaaddir <= hdaaddir;
  hdaaddir <= ('1', '0', 'L', '-');
  hdaaddir <= ('0', '-', '0', '-');
end ehyicv;



-- Seed after: 17370486225618001127,10875537289884587119
