-- Seed: 1962772528360388492,6379010654866854599

entity qaadv is
  port (snyp : buffer bit);
end qaadv;

architecture zlha of qaadv is
  
begin
  -- Single-driven assignments
  snyp <= snyp;
end zlha;

library ieee;
use ieee.std_logic_1164.all;

entity ibtli is
  port (fkny : out severity_level; stkybx : out real; pdiyxqdyl : buffer std_logic_vector(1 to 3));
end ibtli;

architecture vcbwykgbu of ibtli is
  
begin
  -- Multi-driven assignments
  pdiyxqdyl <= ('Z', '1', 'H');
  pdiyxqdyl <= ('0', '0', '1');
end vcbwykgbu;

entity jriabnept is
  port (twhjda : inout string(3 to 2));
end jriabnept;

library ieee;
use ieee.std_logic_1164.all;

architecture hovkfpr of jriabnept is
  signal hfv : bit;
  signal qtjrvr : bit;
  signal xmlbkpwdx : std_logic_vector(1 to 3);
  signal wdjhol : real;
  signal o : severity_level;
begin
  ojxmrco : entity work.ibtli
    port map (fkny => o, stkybx => wdjhol, pdiyxqdyl => xmlbkpwdx);
  zrvkxbqr : entity work.qaadv
    port map (snyp => qtjrvr);
  hojvpqyqbo : entity work.qaadv
    port map (snyp => hfv);
  
  -- Single-driven assignments
  twhjda <= (others => ' ');
end hovkfpr;



-- Seed after: 17344754603072807129,6379010654866854599
