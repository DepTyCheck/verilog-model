-- Seed: 9713015945481867878,6377581586043967497

entity ij is
  port (hphhcgmf : in time_vector(2 downto 1));
end ij;

architecture jgqmpn of ij is
  
begin
  
end jgqmpn;

entity ex is
  port (wcvijo : inout real);
end ex;

architecture tl of ex is
  signal mmkwo : time_vector(2 downto 1);
  signal cw : time_vector(2 downto 1);
begin
  t : entity work.ij
    port map (hphhcgmf => cw);
  p : entity work.ij
    port map (hphhcgmf => cw);
  qljovrozy : entity work.ij
    port map (hphhcgmf => mmkwo);
  
  -- Single-driven assignments
  wcvijo <= 2#0.10#;
  mmkwo <= mmkwo;
  cw <= (0 fs, 3 sec);
end tl;

library ieee;
use ieee.std_logic_1164.all;

entity yt is
  port (c : in std_logic_vector(3 downto 0); jgyh : out std_logic; euzl : buffer integer);
end yt;

architecture bc of yt is
  signal erd : time_vector(2 downto 1);
  signal dvjcqsvj : time_vector(2 downto 1);
begin
  drc : entity work.ij
    port map (hphhcgmf => dvjcqsvj);
  kvwygoipvl : entity work.ij
    port map (hphhcgmf => erd);
  
  -- Single-driven assignments
  euzl <= 2#01#;
  erd <= dvjcqsvj;
  dvjcqsvj <= dvjcqsvj;
  
  -- Multi-driven assignments
  jgyh <= '-';
  jgyh <= jgyh;
end bc;



-- Seed after: 4245753368042464146,6377581586043967497
