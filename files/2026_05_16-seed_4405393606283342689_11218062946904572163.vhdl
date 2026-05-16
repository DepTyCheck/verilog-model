-- Seed: 4405393606283342689,11218062946904572163



entity gkayywr is
  port (edkkb : buffer severity_level; gougebme : out severity_level; kbdadzrwc : out bit);
end gkayywr;



architecture oarlhjjxjp of gkayywr is
  
begin
  
end oarlhjjxjp;



entity szntou is
  port (nl : in severity_level; rfhcsune : out time; nn : in severity_level);
end szntou;



architecture ifelk of szntou is
  signal osob : bit;
  signal nsfdgzw : severity_level;
  signal snmepph : severity_level;
  signal tkww : bit;
  signal txlsruocf : severity_level;
  signal dpw : severity_level;
  signal yjgk : bit;
  signal jzbujy : severity_level;
  signal e : severity_level;
begin
  p : entity work.gkayywr
    port map (edkkb => e, gougebme => jzbujy, kbdadzrwc => yjgk);
  nhpqp : entity work.gkayywr
    port map (edkkb => dpw, gougebme => txlsruocf, kbdadzrwc => tkww);
  imymg : entity work.gkayywr
    port map (edkkb => snmepph, gougebme => nsfdgzw, kbdadzrwc => osob);
end ifelk;



entity djugwg is
  port (te : inout time);
end djugwg;



architecture vqdxmn of djugwg is
  signal ahy : severity_level;
  signal fclqzkkjr : time;
  signal b : severity_level;
  signal cirj : bit;
  signal olsoq : severity_level;
  signal ftnjryeoo : severity_level;
begin
  ztcwyhq : entity work.szntou
    port map (nl => ftnjryeoo, rfhcsune => te, nn => olsoq);
  ujdwi : entity work.gkayywr
    port map (edkkb => ftnjryeoo, gougebme => olsoq, kbdadzrwc => cirj);
  qhbozp : entity work.szntou
    port map (nl => b, rfhcsune => fclqzkkjr, nn => ahy);
end vqdxmn;



-- Seed after: 5392224887461351279,11218062946904572163
