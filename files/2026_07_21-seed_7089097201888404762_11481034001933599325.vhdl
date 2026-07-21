-- Seed: 7089097201888404762,11481034001933599325

library ieee;
use ieee.std_logic_1164.all;

entity ac is
  port (sur : in std_logic_vector(3 to 4));
end ac;

architecture qgc of ac is
  
begin
  
end qgc;

library ieee;
use ieee.std_logic_1164.all;

entity veaildiind is
  port (exxch : linkage integer; zgl : inout time; tolhht : buffer std_logic);
end veaildiind;

library ieee;
use ieee.std_logic_1164.all;

architecture pnto of veaildiind is
  signal mrqbnh : std_logic_vector(3 to 4);
begin
  vpairu : entity work.ac
    port map (sur => mrqbnh);
  ybkg : entity work.ac
    port map (sur => mrqbnh);
  
  -- Single-driven assignments
  zgl <= zgl;
end pnto;

library ieee;
use ieee.std_logic_1164.all;

entity dlsycja is
  port (bsj : in std_logic; fwyqdhbnpe : buffer real; tppzm : linkage real);
end dlsycja;

library ieee;
use ieee.std_logic_1164.all;

architecture xpyqgalzsd of dlsycja is
  signal ocdcdqzp : std_logic_vector(3 to 4);
  signal vrzgm : std_logic;
  signal dvllm : time;
  signal ebdm : integer;
begin
  llfm : entity work.veaildiind
    port map (exxch => ebdm, zgl => dvllm, tolhht => vrzgm);
  owik : entity work.ac
    port map (sur => ocdcdqzp);
  ni : entity work.ac
    port map (sur => ocdcdqzp);
  
  -- Single-driven assignments
  fwyqdhbnpe <= fwyqdhbnpe;
  
  -- Multi-driven assignments
  vrzgm <= bsj;
  vrzgm <= bsj;
end xpyqgalzsd;



-- Seed after: 10403852929927467370,11481034001933599325
