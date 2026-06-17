-- Seed: 464003694638319862,10557070023141912087

entity ejyimax is
  port (gylpddozol : out string(2 to 5); ufqideu : buffer time);
end ejyimax;

architecture nkdmsoebgu of ejyimax is
  
begin
  -- Single-driven assignments
  gylpddozol <= ('m', 'z', 'o', 'v');
  ufqideu <= 0.1 ns;
end nkdmsoebgu;

entity tqyhmv is
  port (gopbx : buffer integer; j : in character; gerqoipo : inout real);
end tqyhmv;

architecture cgyekcshb of tqyhmv is
  signal zkfaxctlqa : time;
  signal zogvuizvje : string(2 to 5);
begin
  gzillcmlh : entity work.ejyimax
    port map (gylpddozol => zogvuizvje, ufqideu => zkfaxctlqa);
  
  -- Single-driven assignments
  gopbx <= 4_2_4_3_3;
end cgyekcshb;

entity hereilfxt is
  port (fgvo : in severity_level);
end hereilfxt;

architecture pasjg of hereilfxt is
  signal a : real;
  signal zb : character;
  signal eu : integer;
  signal nbqtqf : real;
  signal l : character;
  signal vbbjg : integer;
begin
  gmphdk : entity work.tqyhmv
    port map (gopbx => vbbjg, j => l, gerqoipo => nbqtqf);
  hhckqlbx : entity work.tqyhmv
    port map (gopbx => eu, j => zb, gerqoipo => a);
  
  -- Single-driven assignments
  zb <= 'h';
  l <= 'y';
end pasjg;

entity zuamguo is
  port (xoehytw : buffer string(2 to 1); fformhr : buffer real);
end zuamguo;

architecture ryeojzxim of zuamguo is
  signal ndn : time;
  signal suklvwbl : string(2 to 5);
  signal h : time;
  signal nvcmrelehx : string(2 to 5);
begin
  kcckuu : entity work.ejyimax
    port map (gylpddozol => nvcmrelehx, ufqideu => h);
  wts : entity work.ejyimax
    port map (gylpddozol => suklvwbl, ufqideu => ndn);
  
  -- Single-driven assignments
  fformhr <= 16#F.5#;
  xoehytw <= "";
end ryeojzxim;



-- Seed after: 13159081193016598766,10557070023141912087
