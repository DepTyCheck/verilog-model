-- Seed: 2568141720416910800,11181851762153539145

library ieee;
use ieee.std_logic_1164.all;

entity bpsdal is
  port (gguu : out real_vector(1 to 0); emm : in std_logic_vector(4 downto 2); qlbcqczu : buffer integer; rekkbf : in time_vector(4 to 2));
end bpsdal;



architecture qawrcp of bpsdal is
  
begin
  
end qawrcp;



entity itujb is
  port (vvaglzgw : out character);
end itujb;

library ieee;
use ieee.std_logic_1164.all;

architecture hvu of itujb is
  signal menvovbcqg : time_vector(4 to 2);
  signal oj : integer;
  signal nbpwaebgn : std_logic_vector(4 downto 2);
  signal hcybm : real_vector(1 to 0);
  signal gmbewllvc : integer;
  signal wzdphwi : real_vector(1 to 0);
  signal n : integer;
  signal xatedpcjj : std_logic_vector(4 downto 2);
  signal dnlchepfdz : real_vector(1 to 0);
  signal nuimwpymdq : time_vector(4 to 2);
  signal pazt : integer;
  signal jkyw : std_logic_vector(4 downto 2);
  signal ur : real_vector(1 to 0);
begin
  k : entity work.bpsdal
    port map (gguu => ur, emm => jkyw, qlbcqczu => pazt, rekkbf => nuimwpymdq);
  rs : entity work.bpsdal
    port map (gguu => dnlchepfdz, emm => xatedpcjj, qlbcqczu => n, rekkbf => nuimwpymdq);
  wdnna : entity work.bpsdal
    port map (gguu => wzdphwi, emm => xatedpcjj, qlbcqczu => gmbewllvc, rekkbf => nuimwpymdq);
  nzz : entity work.bpsdal
    port map (gguu => hcybm, emm => nbpwaebgn, qlbcqczu => oj, rekkbf => menvovbcqg);
end hvu;



-- Seed after: 10574598264918540436,11181851762153539145
