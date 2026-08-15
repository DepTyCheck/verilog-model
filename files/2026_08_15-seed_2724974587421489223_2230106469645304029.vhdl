-- Seed: 2724974587421489223,2230106469645304029

library ieee;
use ieee.std_logic_1164.all;

entity hu is
  port (esqvucdu : buffer std_logic_vector(1 to 4); sqwq : out real; krmsdqb : inout time);
end hu;

architecture kr of hu is
  
begin
  -- Single-driven assignments
  krmsdqb <= 1 hr;
  sqwq <= sqwq;
  
  -- Multi-driven assignments
  esqvucdu <= esqvucdu;
  esqvucdu <= ('L', 'H', 'Z', 'U');
  esqvucdu <= ('U', 'Z', 'Z', '0');
end kr;

library ieee;
use ieee.std_logic_1164.all;

entity yzd is
  port (zdeaxjj : buffer std_logic_vector(2 downto 0));
end yzd;

library ieee;
use ieee.std_logic_1164.all;

architecture ru of yzd is
  signal ymr : time;
  signal itiiktfwv : real;
  signal unmjlmx : std_logic_vector(1 to 4);
  signal sjkru : time;
  signal pk : real;
  signal q : time;
  signal qnmmloy : real;
  signal g : std_logic_vector(1 to 4);
begin
  jpji : entity work.hu
    port map (esqvucdu => g, sqwq => qnmmloy, krmsdqb => q);
  su : entity work.hu
    port map (esqvucdu => g, sqwq => pk, krmsdqb => sjkru);
  fzicmykepg : entity work.hu
    port map (esqvucdu => unmjlmx, sqwq => itiiktfwv, krmsdqb => ymr);
  
  -- Multi-driven assignments
  zdeaxjj <= ('Z', 'H', 'L');
  zdeaxjj <= "ZL1";
  g <= g;
end ru;

entity eqgvjbs is
  port (lcsa : linkage integer);
end eqgvjbs;

library ieee;
use ieee.std_logic_1164.all;

architecture ewighsbefd of eqgvjbs is
  signal x : time;
  signal mpbnvrvkai : real;
  signal vqbqi : std_logic_vector(1 to 4);
  signal rzdohubrw : std_logic_vector(2 downto 0);
begin
  rpczoixd : entity work.yzd
    port map (zdeaxjj => rzdohubrw);
  gpzrjakbb : entity work.yzd
    port map (zdeaxjj => rzdohubrw);
  l : entity work.hu
    port map (esqvucdu => vqbqi, sqwq => mpbnvrvkai, krmsdqb => x);
  
  -- Multi-driven assignments
  rzdohubrw <= "ZXU";
end ewighsbefd;



-- Seed after: 202363884851809202,2230106469645304029
