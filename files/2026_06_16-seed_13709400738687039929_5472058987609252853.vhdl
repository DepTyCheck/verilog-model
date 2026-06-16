-- Seed: 13709400738687039929,5472058987609252853

entity jew is
  port (drx : linkage time_vector(0 to 1); flrkzohbm : out real_vector(4 downto 0); fnhefee : buffer integer; nzmeyp : buffer boolean_vector(0 to 2));
end jew;

architecture efpegpmu of jew is
  
begin
  
end efpegpmu;

library ieee;
use ieee.std_logic_1164.all;

entity ng is
  port (lbrp : in real; ugstlqxe : inout time_vector(4 downto 1); gtpfpgx : inout std_logic_vector(4 to 3); o : linkage integer);
end ng;

architecture mowba of ng is
  signal c : boolean_vector(0 to 2);
  signal gzxxl : integer;
  signal cpwebkmlyt : real_vector(4 downto 0);
  signal qvabfl : time_vector(0 to 1);
  signal ny : boolean_vector(0 to 2);
  signal izsrvl : integer;
  signal amneqzwg : real_vector(4 downto 0);
  signal ndil : time_vector(0 to 1);
  signal wjwdi : boolean_vector(0 to 2);
  signal phmoblcsky : integer;
  signal tsdxrsw : real_vector(4 downto 0);
  signal jegjne : time_vector(0 to 1);
  signal jj : boolean_vector(0 to 2);
  signal mvxvmndo : integer;
  signal ozcktm : real_vector(4 downto 0);
  signal fmieuvxze : time_vector(0 to 1);
begin
  jogjvgr : entity work.jew
    port map (drx => fmieuvxze, flrkzohbm => ozcktm, fnhefee => mvxvmndo, nzmeyp => jj);
  bqtxk : entity work.jew
    port map (drx => jegjne, flrkzohbm => tsdxrsw, fnhefee => phmoblcsky, nzmeyp => wjwdi);
  x : entity work.jew
    port map (drx => ndil, flrkzohbm => amneqzwg, fnhefee => izsrvl, nzmeyp => ny);
  lgxtu : entity work.jew
    port map (drx => qvabfl, flrkzohbm => cpwebkmlyt, fnhefee => gzxxl, nzmeyp => c);
  
  -- Single-driven assignments
  ugstlqxe <= (8#60642.2# ns, 16#F_A_A_C_7.D_A_A_7_5# ns, 2#1011# fs, 4_3 us);
  
  -- Multi-driven assignments
  gtpfpgx <= (others => '0');
  gtpfpgx <= (others => '0');
  gtpfpgx <= "";
  gtpfpgx <= "";
end mowba;



-- Seed after: 15961582943487172888,5472058987609252853
