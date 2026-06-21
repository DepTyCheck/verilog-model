-- Seed: 17916446849731252520,3687118713772291287

entity npnijprhp is
  port (bnniyplu : linkage time; yxkcy : linkage integer; i : inout time);
end npnijprhp;

architecture jrqbjoudet of npnijprhp is
  
begin
  -- Single-driven assignments
  i <= 2#0_0_0_1.0_0_0_1_0# ns;
end jrqbjoudet;

entity sqxvxrbkcb is
  port (p : linkage time; gmtmm : linkage integer; ooobiw : out real);
end sqxvxrbkcb;

architecture xwobpxlba of sqxvxrbkcb is
  signal fqglp : time;
  signal fctbfkf : integer;
  signal vix : time;
  signal kpxsak : integer;
  signal dsasffmj : time;
  signal nfc : time;
  signal swhrhz : time;
  signal cqe : time;
  signal o : integer;
  signal orlgvwxeaa : time;
begin
  tqsbuqkbc : entity work.npnijprhp
    port map (bnniyplu => orlgvwxeaa, yxkcy => o, i => cqe);
  zjjt : entity work.npnijprhp
    port map (bnniyplu => swhrhz, yxkcy => gmtmm, i => nfc);
  kazgpe : entity work.npnijprhp
    port map (bnniyplu => dsasffmj, yxkcy => kpxsak, i => vix);
  dwg : entity work.npnijprhp
    port map (bnniyplu => p, yxkcy => fctbfkf, i => fqglp);
  
  -- Single-driven assignments
  ooobiw <= 8#4_3.1_4#;
end xwobpxlba;

entity ttkmyy is
  port (nuzk : buffer bit_vector(4 downto 2));
end ttkmyy;

architecture ukj of ttkmyy is
  signal ircsy : time;
  signal sojehl : integer;
  signal dasvvwk : time;
  signal qmtppgp : time;
  signal jtjtyrrwfg : integer;
  signal wfejgcgbcz : time;
  signal upe : time;
  signal flq : integer;
  signal zac : time;
  signal ryoviq : time;
  signal nwjle : integer;
  signal lhw : time;
begin
  pasypidzto : entity work.npnijprhp
    port map (bnniyplu => lhw, yxkcy => nwjle, i => ryoviq);
  aloiraed : entity work.npnijprhp
    port map (bnniyplu => zac, yxkcy => flq, i => upe);
  z : entity work.npnijprhp
    port map (bnniyplu => wfejgcgbcz, yxkcy => jtjtyrrwfg, i => qmtppgp);
  l : entity work.npnijprhp
    port map (bnniyplu => dasvvwk, yxkcy => sojehl, i => ircsy);
  
  -- Single-driven assignments
  nuzk <= ('1', '0', '0');
end ukj;

entity qyc is
  port (lr : buffer boolean_vector(0 downto 0));
end qyc;

architecture vomwqufbb of qyc is
  signal wkajy : bit_vector(4 downto 2);
  signal jfowa : time;
  signal lgrx : integer;
  signal vnqufiqvid : time;
begin
  rfy : entity work.npnijprhp
    port map (bnniyplu => vnqufiqvid, yxkcy => lgrx, i => jfowa);
  wltqnlp : entity work.ttkmyy
    port map (nuzk => wkajy);
end vomwqufbb;



-- Seed after: 9936494433455225166,3687118713772291287
