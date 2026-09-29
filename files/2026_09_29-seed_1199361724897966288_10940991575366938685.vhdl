-- Seed: 1199361724897966288,10940991575366938685

entity pa is
  port (i : in string(3 to 5); tnghpoqln : inout integer; xwkcur : out time; dgdkvnr : out character);
end pa;

architecture ylmfnwddh of pa is
  
begin
  -- Single-driven assignments
  xwkcur <= 2 min;
  tnghpoqln <= 34310;
  dgdkvnr <= 'p';
end ylmfnwddh;

entity mvhmdajnr is
  port (kw : in real);
end mvhmdajnr;

architecture joebflkuad of mvhmdajnr is
  signal s : character;
  signal j : time;
  signal ejwanh : integer;
  signal jxrgxtqh : character;
  signal obwrcftbj : time;
  signal pam : integer;
  signal eraagc : string(3 to 5);
begin
  wpjzzsjvn : entity work.pa
    port map (i => eraagc, tnghpoqln => pam, xwkcur => obwrcftbj, dgdkvnr => jxrgxtqh);
  gbdy : entity work.pa
    port map (i => eraagc, tnghpoqln => ejwanh, xwkcur => j, dgdkvnr => s);
  
  -- Single-driven assignments
  eraagc <= ('x', 'a', 'x');
end joebflkuad;

entity qoz is
  port (hkgrvm : buffer boolean);
end qoz;

architecture nlru of qoz is
  signal ufkusesu : character;
  signal khfngidcc : time;
  signal eeqjopkk : integer;
  signal pplcpz : string(3 to 5);
  signal x : character;
  signal pdzch : time;
  signal ceyzgzmoy : integer;
  signal rqvwrt : string(3 to 5);
  signal l : character;
  signal ipatwlpfdx : time;
  signal vnjapdgdlj : integer;
  signal axmsi : string(3 to 5);
begin
  zkgymfp : entity work.pa
    port map (i => axmsi, tnghpoqln => vnjapdgdlj, xwkcur => ipatwlpfdx, dgdkvnr => l);
  v : entity work.pa
    port map (i => rqvwrt, tnghpoqln => ceyzgzmoy, xwkcur => pdzch, dgdkvnr => x);
  wbicxgcely : entity work.pa
    port map (i => pplcpz, tnghpoqln => eeqjopkk, xwkcur => khfngidcc, dgdkvnr => ufkusesu);
end nlru;

entity hnkmktbygf is
  port (hjjssvz : inout integer);
end hnkmktbygf;

architecture cqbajpqn of hnkmktbygf is
  signal gygjedx : boolean;
  signal jdhu : real;
  signal koay : real;
  signal vbwxod : character;
  signal c : time;
  signal ksail : string(3 to 5);
begin
  v : entity work.pa
    port map (i => ksail, tnghpoqln => hjjssvz, xwkcur => c, dgdkvnr => vbwxod);
  ccmpldzakq : entity work.mvhmdajnr
    port map (kw => koay);
  mmjdwav : entity work.mvhmdajnr
    port map (kw => jdhu);
  uveteyrqc : entity work.qoz
    port map (hkgrvm => gygjedx);
end cqbajpqn;



-- Seed after: 2864527967824890718,10940991575366938685
