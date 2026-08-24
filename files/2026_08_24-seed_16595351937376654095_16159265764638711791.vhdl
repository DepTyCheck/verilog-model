-- Seed: 16595351937376654095,16159265764638711791

library ieee;
use ieee.std_logic_1164.all;

entity veoc is
  port ( veibzroe : linkage std_logic_vector(2 to 4)
  ; zeubcvu : buffer integer_vector(0 downto 2)
  ; ju : inout std_logic
  ; fzdd : buffer std_logic_vector(0 to 3)
  );
end veoc;

architecture cgjpenotqi of veoc is
  
begin
  
end cgjpenotqi;

entity qbn is
  port (tvahsprft : out time; esxucvpusk : inout real);
end qbn;

library ieee;
use ieee.std_logic_1164.all;

architecture hrjubpzcdr of qbn is
  signal tbtk : std_logic;
  signal cllqesmkbi : integer_vector(0 downto 2);
  signal yeojc : std_logic_vector(2 to 4);
  signal cf : integer_vector(0 downto 2);
  signal ee : std_logic;
  signal ht : integer_vector(0 downto 2);
  signal puvbdwhuvj : std_logic_vector(0 to 3);
  signal wpmpygfh : std_logic;
  signal sowajw : integer_vector(0 downto 2);
  signal nybgweojn : std_logic_vector(2 to 4);
begin
  uzsvcpq : entity work.veoc
    port map (veibzroe => nybgweojn, zeubcvu => sowajw, ju => wpmpygfh, fzdd => puvbdwhuvj);
  fhc : entity work.veoc
    port map (veibzroe => nybgweojn, zeubcvu => ht, ju => ee, fzdd => puvbdwhuvj);
  zydmcwmrgy : entity work.veoc
    port map (veibzroe => nybgweojn, zeubcvu => cf, ju => wpmpygfh, fzdd => puvbdwhuvj);
  pxfbduoh : entity work.veoc
    port map (veibzroe => yeojc, zeubcvu => cllqesmkbi, ju => tbtk, fzdd => puvbdwhuvj);
  
  -- Single-driven assignments
  esxucvpusk <= esxucvpusk;
  tvahsprft <= tvahsprft;
  
  -- Multi-driven assignments
  nybgweojn <= ('Z', '-', 'U');
  nybgweojn <= nybgweojn;
  yeojc <= nybgweojn;
  ee <= 'L';
end hrjubpzcdr;

library ieee;
use ieee.std_logic_1164.all;

entity pcy is
  port (cazk : in std_logic);
end pcy;

library ieee;
use ieee.std_logic_1164.all;

architecture uqh of pcy is
  signal sskoufecf : integer_vector(0 downto 2);
  signal yzqoowtrf : std_logic_vector(2 to 4);
  signal qfbq : std_logic_vector(0 to 3);
  signal lrocn : std_logic;
  signal phh : integer_vector(0 downto 2);
  signal hajfghrql : std_logic_vector(2 to 4);
begin
  jhk : entity work.veoc
    port map (veibzroe => hajfghrql, zeubcvu => phh, ju => lrocn, fzdd => qfbq);
  dmsvpapo : entity work.veoc
    port map (veibzroe => yzqoowtrf, zeubcvu => sskoufecf, ju => lrocn, fzdd => qfbq);
  
  -- Multi-driven assignments
  qfbq <= qfbq;
  hajfghrql <= ('X', 'U', 'H');
end uqh;

entity irniji is
  port (okubs : out real; wljpoixd : inout real_vector(3 to 1); mdndjgaye : inout time);
end irniji;

library ieee;
use ieee.std_logic_1164.all;

architecture pnjzilbfjp of irniji is
  signal lblgfb : std_logic_vector(0 to 3);
  signal oegdgpqwnc : std_logic;
  signal ndrsjedz : integer_vector(0 downto 2);
  signal hqje : std_logic_vector(2 to 4);
begin
  rmbsrwmmhi : entity work.veoc
    port map (veibzroe => hqje, zeubcvu => ndrsjedz, ju => oegdgpqwnc, fzdd => lblgfb);
  se : entity work.qbn
    port map (tvahsprft => mdndjgaye, esxucvpusk => okubs);
end pnjzilbfjp;



-- Seed after: 7506504999396046731,16159265764638711791
