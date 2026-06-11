-- Seed: 826218167312276136,11181851762153539145



entity etnbol is
  port (tt : in severity_level; pfpgjodl : buffer real; t : buffer character; asjzsx : in time_vector(4 to 1));
end etnbol;



architecture dmqqlxljn of etnbol is
  
begin
  
end dmqqlxljn;

library ieee;
use ieee.std_logic_1164.all;

entity qucpsy is
  port (ixyxb : buffer bit; gju : linkage std_logic; rudub : in integer; ezh : inout real_vector(4 to 4));
end qucpsy;



architecture kmhwp of qucpsy is
  signal mpgn : character;
  signal ifunl : real;
  signal jhcwnof : time_vector(4 to 1);
  signal pwncq : character;
  signal quq : real;
  signal k : time_vector(4 to 1);
  signal vmgrwvaf : character;
  signal qam : real;
  signal nx : severity_level;
begin
  kzog : entity work.etnbol
    port map (tt => nx, pfpgjodl => qam, t => vmgrwvaf, asjzsx => k);
  vtv : entity work.etnbol
    port map (tt => nx, pfpgjodl => quq, t => pwncq, asjzsx => jhcwnof);
  uegminuxm : entity work.etnbol
    port map (tt => nx, pfpgjodl => ifunl, t => mpgn, asjzsx => jhcwnof);
end kmhwp;



entity trqxwmvqdg is
  port (vnea : in integer; qhndlo : in integer; uayyhhld : inout real);
end trqxwmvqdg;



architecture yuh of trqxwmvqdg is
  
begin
  
end yuh;



entity beaviawq is
  port (tongwly : buffer bit; lzrdw : inout character);
end beaviawq;

library ieee;
use ieee.std_logic_1164.all;

architecture fv of beaviawq is
  signal y : time_vector(4 to 1);
  signal icnypufc : character;
  signal lrapv : real;
  signal koupo : time_vector(4 to 1);
  signal jlpwkvf : real;
  signal yldelxxq : severity_level;
  signal sevsnkn : real;
  signal hp : integer;
  signal wmhuafw : real_vector(4 to 4);
  signal ycwooru : integer;
  signal tm : std_logic;
  signal ldnt : bit;
begin
  klmslu : entity work.qucpsy
    port map (ixyxb => ldnt, gju => tm, rudub => ycwooru, ezh => wmhuafw);
  t : entity work.trqxwmvqdg
    port map (vnea => hp, qhndlo => ycwooru, uayyhhld => sevsnkn);
  nu : entity work.etnbol
    port map (tt => yldelxxq, pfpgjodl => jlpwkvf, t => lzrdw, asjzsx => koupo);
  ixz : entity work.etnbol
    port map (tt => yldelxxq, pfpgjodl => lrapv, t => icnypufc, asjzsx => y);
end fv;



-- Seed after: 6302989390526607045,11181851762153539145
