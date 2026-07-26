-- Seed: 13908680290551545514,7808623373429384027

entity cncen is
  port (gvfmekci : inout time; iducpo : inout time);
end cncen;

architecture aobebdouan of cncen is
  
begin
  
end aobebdouan;

entity ansy is
  port (gyyafftnfc : buffer time; fcilsvb : in bit; kymyzzt : linkage time_vector(3 downto 4); hjyvoysg : linkage real);
end ansy;

architecture jwjl of ansy is
  signal fah : time;
  signal bmbfolwah : time;
  signal fmnexjd : time;
begin
  fjjrrmv : entity work.cncen
    port map (gvfmekci => fmnexjd, iducpo => bmbfolwah);
  obgrngd : entity work.cncen
    port map (gvfmekci => fah, iducpo => gyyafftnfc);
end jwjl;

library ieee;
use ieee.std_logic_1164.all;

entity gzshsp is
  port (nitvfm : buffer std_logic; k : linkage integer; ioahpqdeso : in integer);
end gzshsp;

architecture nfxevbnuz of gzshsp is
  signal jydbog : time;
  signal nbahgorso : time;
  signal kzjgfunpw : time;
  signal qacw : time;
  signal fem : time;
  signal crqsy : time;
  signal laeszw : time;
  signal i : time;
begin
  pytc : entity work.cncen
    port map (gvfmekci => i, iducpo => laeszw);
  cxafuobaif : entity work.cncen
    port map (gvfmekci => crqsy, iducpo => fem);
  yebdssfz : entity work.cncen
    port map (gvfmekci => qacw, iducpo => kzjgfunpw);
  lhod : entity work.cncen
    port map (gvfmekci => nbahgorso, iducpo => jydbog);
  
  -- Multi-driven assignments
  nitvfm <= nitvfm;
end nfxevbnuz;

entity iqmk is
  port (argszkls : linkage time);
end iqmk;

library ieee;
use ieee.std_logic_1164.all;

architecture puafe of iqmk is
  signal gkeh : integer;
  signal rerq : time;
  signal csyfawtmor : time;
  signal cuariz : time;
  signal jso : time;
  signal n : integer;
  signal xszxaeh : integer;
  signal ygba : std_logic;
begin
  ueedjldkv : entity work.gzshsp
    port map (nitvfm => ygba, k => xszxaeh, ioahpqdeso => n);
  isdgbchcsy : entity work.cncen
    port map (gvfmekci => jso, iducpo => cuariz);
  qmjjc : entity work.cncen
    port map (gvfmekci => csyfawtmor, iducpo => rerq);
  qbyij : entity work.gzshsp
    port map (nitvfm => ygba, k => gkeh, ioahpqdeso => xszxaeh);
  
  -- Single-driven assignments
  n <= 2#0000#;
  
  -- Multi-driven assignments
  ygba <= 'X';
end puafe;



-- Seed after: 5596052506077221966,7808623373429384027
