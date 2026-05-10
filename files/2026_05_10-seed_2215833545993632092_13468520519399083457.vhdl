-- Seed: 2215833545993632092,13468520519399083457



entity l is
  port (hpnpzxz : in time; xiyiruh : inout time);
end l;



architecture usildule of l is
  
begin
  
end usildule;



entity btla is
  port (mlryjv : inout time; tvreej : in time);
end btla;



architecture ozd of btla is
  signal lc : time;
  signal urdmypus : time;
begin
  k : entity work.l
    port map (hpnpzxz => urdmypus, xiyiruh => lc);
  gvbqjqvxv : entity work.l
    port map (hpnpzxz => tvreej, xiyiruh => mlryjv);
end ozd;



entity qcrmjtjarh is
  port (gadjig : in real; uqbyymp : out integer; wkrbko : out integer; nw : in boolean);
end qcrmjtjarh;



architecture vfwr of qcrmjtjarh is
  signal wzvxm : time;
  signal lavtcy : time;
  signal akcj : time;
  signal qkqxcnhx : time;
  signal gqs : time;
begin
  yqn : entity work.l
    port map (hpnpzxz => gqs, xiyiruh => qkqxcnhx);
  baphcmhltq : entity work.btla
    port map (mlryjv => gqs, tvreej => akcj);
  j : entity work.l
    port map (hpnpzxz => lavtcy, xiyiruh => akcj);
  qjfk : entity work.btla
    port map (mlryjv => lavtcy, tvreej => wzvxm);
end vfwr;



-- Seed after: 9025163154835261177,13468520519399083457
