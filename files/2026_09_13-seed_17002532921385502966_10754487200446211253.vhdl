-- Seed: 17002532921385502966,10754487200446211253

library ieee;
use ieee.std_logic_1164.all;

entity lw is
  port (hiofm : buffer std_logic_vector(1 to 2); ze : buffer std_logic_vector(2 to 3));
end lw;

architecture euylmzfeo of lw is
  
begin
  -- Multi-driven assignments
  ze <= ze;
  ze <= ze;
  hiofm <= ('U', '1');
  ze <= ze;
end euylmzfeo;

entity fwfjzqhj is
  port (cyppcupqu : in integer; q : in integer; xzwcftlul : inout bit);
end fwfjzqhj;

library ieee;
use ieee.std_logic_1164.all;

architecture bsnoz of fwfjzqhj is
  signal twklorzk : std_logic_vector(1 to 2);
  signal eqtsk : std_logic_vector(1 to 2);
  signal uk : std_logic_vector(1 to 2);
  signal yjz : std_logic_vector(2 to 3);
  signal pcpvljtvn : std_logic_vector(2 to 3);
begin
  wmif : entity work.lw
    port map (hiofm => pcpvljtvn, ze => yjz);
  uleqnqlot : entity work.lw
    port map (hiofm => uk, ze => eqtsk);
  oszohkykg : entity work.lw
    port map (hiofm => twklorzk, ze => pcpvljtvn);
  nypkusxqb : entity work.lw
    port map (hiofm => eqtsk, ze => pcpvljtvn);
  
  -- Single-driven assignments
  xzwcftlul <= '1';
  
  -- Multi-driven assignments
  pcpvljtvn <= pcpvljtvn;
end bsnoz;



-- Seed after: 2914456520291853442,10754487200446211253
