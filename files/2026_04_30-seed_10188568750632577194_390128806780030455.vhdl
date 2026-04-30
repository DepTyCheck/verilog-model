-- Seed: 10188568750632577194,390128806780030455



entity xqx is
  port (jfwsy : in severity_level; hqtmutc : buffer integer; nfmpavyo : buffer integer);
end xqx;



architecture ojdaic of xqx is
  
begin
  
end ojdaic;

library ieee;
use ieee.std_logic_1164.all;

entity kv is
  port (sqtmlr : in std_logic; qlifh : inout time);
end kv;



architecture px of kv is
  signal osuisatap : integer;
  signal ekg : integer;
  signal rfi : integer;
  signal sbqdsvlm : integer;
  signal acoz : severity_level;
begin
  efcv : entity work.xqx
    port map (jfwsy => acoz, hqtmutc => sbqdsvlm, nfmpavyo => rfi);
  egmrmqi : entity work.xqx
    port map (jfwsy => acoz, hqtmutc => ekg, nfmpavyo => osuisatap);
end px;



entity hhu is
  port (zofix : out time; iehkoextaz : linkage time; hf : linkage bit; ghvuar : inout integer);
end hhu;

library ieee;
use ieee.std_logic_1164.all;

architecture hkktrb of hhu is
  signal eo : time;
  signal nohevxansz : std_logic;
  signal bpc : integer;
  signal zuyagfrcmy : integer;
  signal zchl : severity_level;
  signal vil : integer;
  signal vnx : severity_level;
  signal nwwkbywf : time;
  signal wmpunzx : std_logic;
begin
  bultyhnid : entity work.kv
    port map (sqtmlr => wmpunzx, qlifh => nwwkbywf);
  rgxtwhbjf : entity work.xqx
    port map (jfwsy => vnx, hqtmutc => vil, nfmpavyo => ghvuar);
  w : entity work.xqx
    port map (jfwsy => zchl, hqtmutc => zuyagfrcmy, nfmpavyo => bpc);
  dr : entity work.kv
    port map (sqtmlr => nohevxansz, qlifh => eo);
end hkktrb;



-- Seed after: 7618829481049200338,390128806780030455
