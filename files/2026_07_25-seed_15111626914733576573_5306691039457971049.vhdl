-- Seed: 15111626914733576573,5306691039457971049

entity esmuzfy is
  port (fqn : linkage character);
end esmuzfy;

architecture mzpdczjf of esmuzfy is
  
begin
  
end mzpdczjf;

entity ihld is
  port (i : inout real; abwxp : inout boolean; pgynxozlw : in time);
end ihld;

architecture es of ihld is
  signal bzjqeobdjn : character;
  signal dbowqvsdqz : character;
  signal ytaymmxjs : character;
begin
  akiautni : entity work.esmuzfy
    port map (fqn => ytaymmxjs);
  yrark : entity work.esmuzfy
    port map (fqn => dbowqvsdqz);
  uwziyk : entity work.esmuzfy
    port map (fqn => bzjqeobdjn);
end es;

entity nemmrsxrjt is
  port (xzfwg : out time; jri : out bit; zbwcanxf : buffer integer; qfw : buffer integer);
end nemmrsxrjt;

architecture wgtya of nemmrsxrjt is
  signal fendnsi : time;
  signal ee : boolean;
  signal sypcffy : real;
  signal utnecl : boolean;
  signal cabeonymsl : real;
  signal nemxp : time;
  signal lqqx : boolean;
  signal mvulvrwuvk : real;
  signal yirlbr : character;
begin
  ppccj : entity work.esmuzfy
    port map (fqn => yirlbr);
  gvqcpigvo : entity work.ihld
    port map (i => mvulvrwuvk, abwxp => lqqx, pgynxozlw => nemxp);
  xa : entity work.ihld
    port map (i => cabeonymsl, abwxp => utnecl, pgynxozlw => nemxp);
  bnszcdq : entity work.ihld
    port map (i => sypcffy, abwxp => ee, pgynxozlw => fendnsi);
  
  -- Single-driven assignments
  jri <= '0';
  fendnsi <= 8#0_0_4_4_1.6_2_4_6_6# fs;
  xzfwg <= xzfwg;
  zbwcanxf <= 3;
  qfw <= qfw;
end wgtya;



-- Seed after: 15286961910290452451,5306691039457971049
