-- Seed: 6150468003164741018,7142793346053417159



entity mraretvwuk is
  port (kcwuea : out integer; oafhezaoy : linkage time; sktx : out boolean_vector(3 downto 4));
end mraretvwuk;



architecture muytujywej of mraretvwuk is
  
begin
  
end muytujywej;



entity guebf is
  port (hgkia : in real; irs : in integer);
end guebf;



architecture wooznythbm of guebf is
  
begin
  
end wooznythbm;



entity svjpq is
  port (xclnpgst : inout severity_level);
end svjpq;



architecture kdximidztv of svjpq is
  signal h : boolean_vector(3 downto 4);
  signal pjmie : time;
  signal ztxic : integer;
  signal oetw : integer;
  signal vlulxirzpw : real;
begin
  t : entity work.guebf
    port map (hgkia => vlulxirzpw, irs => oetw);
  snonuqu : entity work.mraretvwuk
    port map (kcwuea => ztxic, oafhezaoy => pjmie, sktx => h);
end kdximidztv;

library ieee;
use ieee.std_logic_1164.all;

entity ftkolne is
  port (wq : inout real; nghxrtn : inout integer; mo : inout std_logic_vector(3 to 4));
end ftkolne;



architecture ot of ftkolne is
  signal zunqlqkton : boolean_vector(3 downto 4);
  signal hrkedto : boolean_vector(3 downto 4);
  signal y : integer;
  signal yrjqhnlsxc : boolean_vector(3 downto 4);
  signal ym : time;
  signal lokxid : integer;
begin
  gvyy : entity work.mraretvwuk
    port map (kcwuea => lokxid, oafhezaoy => ym, sktx => yrjqhnlsxc);
  hn : entity work.mraretvwuk
    port map (kcwuea => y, oafhezaoy => ym, sktx => hrkedto);
  mufmqg : entity work.mraretvwuk
    port map (kcwuea => nghxrtn, oafhezaoy => ym, sktx => zunqlqkton);
end ot;



-- Seed after: 7208628101816621120,7142793346053417159
