-- Seed: 6264301276291325668,17219977430298397867

entity mdgfp is
  port (ehymnjzutc : in bit_vector(4 downto 1));
end mdgfp;

architecture dwy of mdgfp is
  
begin
  
end dwy;

library ieee;
use ieee.std_logic_1164.all;

entity iwtkepr is
  port (tlmcisdd : out bit; k : buffer std_logic_vector(3 downto 0); rbacoqcwe : buffer bit);
end iwtkepr;

architecture xdbtdp of iwtkepr is
  signal wjgbwfpb : bit_vector(4 downto 1);
  signal gejmvcnvtb : bit_vector(4 downto 1);
begin
  r : entity work.mdgfp
    port map (ehymnjzutc => gejmvcnvtb);
  bibtvnof : entity work.mdgfp
    port map (ehymnjzutc => gejmvcnvtb);
  glzebr : entity work.mdgfp
    port map (ehymnjzutc => wjgbwfpb);
  
  -- Single-driven assignments
  gejmvcnvtb <= gejmvcnvtb;
  rbacoqcwe <= tlmcisdd;
  tlmcisdd <= '0';
  
  -- Multi-driven assignments
  k <= ('X', '0', '1', 'W');
  k <= ('L', '-', 'W', '1');
end xdbtdp;



-- Seed after: 6866719783259590230,17219977430298397867
