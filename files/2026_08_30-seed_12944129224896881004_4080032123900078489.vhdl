-- Seed: 12944129224896881004,4080032123900078489

entity zecuy is
  port (xzukijqxdt : buffer integer_vector(1 to 3); ky : linkage boolean);
end zecuy;

architecture elwenj of zecuy is
  
begin
  -- Single-driven assignments
  xzukijqxdt <= (3_0_0_1_2, 2#0_1_0_0#, 8#215#);
end elwenj;

library ieee;
use ieee.std_logic_1164.all;

entity sbeudlzo is
  port (a : out std_logic; gdvzoy : inout bit; lugxhn : out std_logic_vector(4 to 4));
end sbeudlzo;

architecture koshpa of sbeudlzo is
  signal xyqtjke : boolean;
  signal gh : integer_vector(1 to 3);
  signal xiwms : boolean;
  signal uihguuia : integer_vector(1 to 3);
  signal sgdb : boolean;
  signal hd : integer_vector(1 to 3);
  signal b : boolean;
  signal hocqpg : integer_vector(1 to 3);
begin
  wdejrkz : entity work.zecuy
    port map (xzukijqxdt => hocqpg, ky => b);
  vxeyywj : entity work.zecuy
    port map (xzukijqxdt => hd, ky => sgdb);
  iqyxyhcuu : entity work.zecuy
    port map (xzukijqxdt => uihguuia, ky => xiwms);
  bqpfxgx : entity work.zecuy
    port map (xzukijqxdt => gh, ky => xyqtjke);
  
  -- Multi-driven assignments
  lugxhn <= "Z";
  lugxhn <= "L";
end koshpa;

entity bjt is
  port (tvew : inout severity_level);
end bjt;

library ieee;
use ieee.std_logic_1164.all;

architecture zqmtmphhii of bjt is
  signal edcahz : std_logic_vector(4 to 4);
  signal xvzt : bit;
  signal iqz : std_logic;
  signal adjkzi : boolean;
  signal wetz : integer_vector(1 to 3);
  signal vdpgbtp : boolean;
  signal ynpjypw : integer_vector(1 to 3);
  signal tpdfenghhd : boolean;
  signal nhnzblrfv : integer_vector(1 to 3);
begin
  m : entity work.zecuy
    port map (xzukijqxdt => nhnzblrfv, ky => tpdfenghhd);
  kbtdgjxqic : entity work.zecuy
    port map (xzukijqxdt => ynpjypw, ky => vdpgbtp);
  divasrmagj : entity work.zecuy
    port map (xzukijqxdt => wetz, ky => adjkzi);
  ysnlwypscw : entity work.sbeudlzo
    port map (a => iqz, gdvzoy => xvzt, lugxhn => edcahz);
  
  -- Single-driven assignments
  tvew <= tvew;
  
  -- Multi-driven assignments
  iqz <= iqz;
  iqz <= iqz;
  iqz <= 'H';
  iqz <= iqz;
end zqmtmphhii;



-- Seed after: 4840669221787512630,4080032123900078489
