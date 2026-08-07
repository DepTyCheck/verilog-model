-- Seed: 11680840326672827548,8068158652091157513

entity kycjonursv is
  port (qz : in character; rcapaenyj : out time_vector(4 to 1); yyggvyrqh : buffer real);
end kycjonursv;

architecture uecysihb of kycjonursv is
  
begin
  -- Single-driven assignments
  yyggvyrqh <= yyggvyrqh;
  rcapaenyj <= rcapaenyj;
end uecysihb;

library ieee;
use ieee.std_logic_1164.all;

entity sxbetc is
  port (um : out real; gfgts : inout real; vmoaunl : buffer std_logic; dpjjzu : out real);
end sxbetc;

architecture aoq of sxbetc is
  signal ag : real;
  signal rcwcuowygo : time_vector(4 to 1);
  signal wzx : real;
  signal gapqxf : time_vector(4 to 1);
  signal gxd : character;
begin
  j : entity work.kycjonursv
    port map (qz => gxd, rcapaenyj => gapqxf, yyggvyrqh => wzx);
  jolohxqwhy : entity work.kycjonursv
    port map (qz => gxd, rcapaenyj => rcwcuowygo, yyggvyrqh => ag);
  
  -- Multi-driven assignments
  vmoaunl <= 'L';
  vmoaunl <= '1';
  vmoaunl <= vmoaunl;
end aoq;

library ieee;
use ieee.std_logic_1164.all;

entity tra is
  port (rgup : in std_logic_vector(3 to 4); xvdylqobu : linkage time_vector(2 to 0); unpx : linkage integer);
end tra;

library ieee;
use ieee.std_logic_1164.all;

architecture mihdr of tra is
  signal zhyyviw : real;
  signal tb : std_logic;
  signal hadtcbp : real;
  signal jqbx : real;
begin
  upanbog : entity work.sxbetc
    port map (um => jqbx, gfgts => hadtcbp, vmoaunl => tb, dpjjzu => zhyyviw);
  
  -- Multi-driven assignments
  tb <= 'W';
  tb <= tb;
end mihdr;

entity ntpq is
  port (eciadncndt : in time; lmbwx : in time; xfplt : inout integer);
end ntpq;

library ieee;
use ieee.std_logic_1164.all;

architecture czoayruz of ntpq is
  signal sofwlofrtr : integer;
  signal likqtlg : time_vector(2 to 0);
  signal gqjuv : time_vector(2 to 0);
  signal fjduggtnj : integer;
  signal bmm : time_vector(2 to 0);
  signal praands : std_logic_vector(3 to 4);
  signal uxenz : integer;
  signal lrslprjkf : time_vector(2 to 0);
  signal rifdgkwho : std_logic_vector(3 to 4);
begin
  wymrhxf : entity work.tra
    port map (rgup => rifdgkwho, xvdylqobu => lrslprjkf, unpx => uxenz);
  vdtmfuirk : entity work.tra
    port map (rgup => praands, xvdylqobu => bmm, unpx => fjduggtnj);
  vvgmbzzck : entity work.tra
    port map (rgup => praands, xvdylqobu => gqjuv, unpx => xfplt);
  gfr : entity work.tra
    port map (rgup => praands, xvdylqobu => likqtlg, unpx => sofwlofrtr);
  
  -- Multi-driven assignments
  rifdgkwho <= "0U";
  rifdgkwho <= ('1', 'Z');
  praands <= rifdgkwho;
  rifdgkwho <= rifdgkwho;
end czoayruz;



-- Seed after: 5595087630800433550,8068158652091157513
