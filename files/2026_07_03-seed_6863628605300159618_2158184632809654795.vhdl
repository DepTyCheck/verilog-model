-- Seed: 6863628605300159618,2158184632809654795

entity bsh is
  port (wvcnmdnzro : out boolean_vector(4 downto 2));
end bsh;

architecture sqj of bsh is
  
begin
  -- Single-driven assignments
  wvcnmdnzro <= (TRUE, TRUE, TRUE);
end sqj;

entity l is
  port (mbhcosnv : linkage time; xcsy : out boolean; cstvmjjfka : linkage real; xqq : inout real);
end l;

architecture a of l is
  signal xvwhjhli : boolean_vector(4 downto 2);
  signal dgofiuejm : boolean_vector(4 downto 2);
  signal wblqzmfx : boolean_vector(4 downto 2);
begin
  gbqbur : entity work.bsh
    port map (wvcnmdnzro => wblqzmfx);
  qztrquslhp : entity work.bsh
    port map (wvcnmdnzro => dgofiuejm);
  exgzq : entity work.bsh
    port map (wvcnmdnzro => xvwhjhli);
  
  -- Single-driven assignments
  xcsy <= TRUE;
  xqq <= 16#D_E.9_4#;
end a;

use std.reflection.all;

entity dlniaxkne is
  port (zpzgwq : out severity_level; wobovjkk : inout subtype_mirror; t : inout integer_subtype_mirror);
end dlniaxkne;

architecture leqnqu of dlniaxkne is
  signal vvnyvblha : boolean_vector(4 downto 2);
  signal rexdlx : boolean_vector(4 downto 2);
  signal mbjjsbqjj : real;
  signal htpeyzhj : real;
  signal etskkp : boolean;
  signal fsxnlbflhz : time;
begin
  qscejtjm : entity work.l
    port map (mbhcosnv => fsxnlbflhz, xcsy => etskkp, cstvmjjfka => htpeyzhj, xqq => mbjjsbqjj);
  rebqu : entity work.bsh
    port map (wvcnmdnzro => rexdlx);
  oeyn : entity work.bsh
    port map (wvcnmdnzro => vvnyvblha);
  
  -- Single-driven assignments
  zpzgwq <= zpzgwq;
end leqnqu;



-- Seed after: 14905400435999570294,2158184632809654795
