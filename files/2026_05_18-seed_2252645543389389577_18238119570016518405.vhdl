-- Seed: 2252645543389389577,18238119570016518405



entity wvws is
  port (t : buffer bit_vector(3 to 3); dbw : out character; sym : in integer; ipgd : linkage character);
end wvws;



architecture kzxqndiee of wvws is
  
begin
  
end kzxqndiee;



entity upgcnul is
  port (koxx : out integer_vector(1 downto 1); u : linkage time);
end upgcnul;



architecture tusqvczw of upgcnul is
  signal oazipkbv : integer;
  signal eq : bit_vector(3 to 3);
  signal nxh : character;
  signal t : integer;
  signal g : character;
  signal qil : bit_vector(3 to 3);
  signal mnmtbeq : character;
  signal oepuqrrw : integer;
  signal uflmzqnuli : character;
  signal hw : bit_vector(3 to 3);
begin
  j : entity work.wvws
    port map (t => hw, dbw => uflmzqnuli, sym => oepuqrrw, ipgd => mnmtbeq);
  r : entity work.wvws
    port map (t => qil, dbw => g, sym => t, ipgd => nxh);
  ntifjhtaya : entity work.wvws
    port map (t => eq, dbw => mnmtbeq, sym => oazipkbv, ipgd => uflmzqnuli);
end tusqvczw;



entity zcqrfbust is
  port (uqh : out integer; gwulfso : out boolean);
end zcqrfbust;



architecture oqkmzotpyf of zcqrfbust is
  signal mxsfbw : time;
  signal zcr : integer_vector(1 downto 1);
  signal noaca : character;
  signal clwm : integer;
  signal tabqzmt : bit_vector(3 to 3);
  signal hbzqbsdwb : character;
  signal wcqhtgy : integer;
  signal puebgqnx : character;
  signal tzhzjceyph : bit_vector(3 to 3);
begin
  xc : entity work.wvws
    port map (t => tzhzjceyph, dbw => puebgqnx, sym => wcqhtgy, ipgd => hbzqbsdwb);
  mexdih : entity work.wvws
    port map (t => tabqzmt, dbw => hbzqbsdwb, sym => clwm, ipgd => noaca);
  f : entity work.upgcnul
    port map (koxx => zcr, u => mxsfbw);
end oqkmzotpyf;



-- Seed after: 18213470093752366375,18238119570016518405
