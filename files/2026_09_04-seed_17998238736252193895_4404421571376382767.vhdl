-- Seed: 17998238736252193895,4404421571376382767

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (moydcxhue : out real_vector(4 to 1); xblc : in std_logic);
end y;

architecture cqxyyrdpya of y is
  
begin
  -- Single-driven assignments
  moydcxhue <= (others => 0.0);
end cqxyyrdpya;

library ieee;
use ieee.std_logic_1164.all;

entity auhmwt is
  port (uw : out integer; gl : out time; ffudheqsy : in std_logic);
end auhmwt;

architecture nk of auhmwt is
  
begin
  -- Single-driven assignments
  gl <= 1 hr;
  uw <= uw;
end nk;

entity yrqtkzj is
  port (pyc : in integer);
end yrqtkzj;

library ieee;
use ieee.std_logic_1164.all;

architecture gaahvagadu of yrqtkzj is
  signal zhq : real_vector(4 to 1);
  signal s : std_logic;
  signal qnzvvuvtq : time;
  signal rlwpse : integer;
  signal biqwcl : real_vector(4 to 1);
  signal ufqymsq : std_logic;
  signal rptxoesoj : real_vector(4 to 1);
begin
  bzswbvms : entity work.y
    port map (moydcxhue => rptxoesoj, xblc => ufqymsq);
  jgryhctg : entity work.y
    port map (moydcxhue => biqwcl, xblc => ufqymsq);
  vgrwp : entity work.auhmwt
    port map (uw => rlwpse, gl => qnzvvuvtq, ffudheqsy => s);
  glpigshse : entity work.y
    port map (moydcxhue => zhq, xblc => ufqymsq);
  
  -- Multi-driven assignments
  ufqymsq <= 'U';
  s <= ufqymsq;
  ufqymsq <= s;
end gaahvagadu;



-- Seed after: 12933902527643752745,4404421571376382767
