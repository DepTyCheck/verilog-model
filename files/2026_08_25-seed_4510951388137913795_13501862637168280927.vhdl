-- Seed: 4510951388137913795,13501862637168280927

entity su is
  port (m : buffer integer; rtdqq : out bit);
end su;

architecture x of su is
  
begin
  -- Single-driven assignments
  m <= 0;
  rtdqq <= '1';
end x;

library ieee;
use ieee.std_logic_1164.all;

entity vzmrr is
  port (fdx : buffer std_logic_vector(3 to 2));
end vzmrr;

architecture czwxhtx of vzmrr is
  signal ntr : bit;
  signal iiexdf : integer;
  signal fzrfnwcmee : bit;
  signal rkjuqy : integer;
begin
  apndht : entity work.su
    port map (m => rkjuqy, rtdqq => fzrfnwcmee);
  wtxw : entity work.su
    port map (m => iiexdf, rtdqq => ntr);
  
  -- Multi-driven assignments
  fdx <= fdx;
end czwxhtx;



-- Seed after: 16889177737850791262,13501862637168280927
