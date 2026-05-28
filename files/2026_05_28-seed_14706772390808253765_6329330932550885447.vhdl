-- Seed: 14706772390808253765,6329330932550885447

library ieee;
use ieee.std_logic_1164.all;

entity xkbf is
  port (srickcd : in real; gvjvolwd : inout boolean_vector(3 to 0); xnju : out real; vniuynayh : linkage std_logic_vector(2 to 0));
end xkbf;



architecture tom of xkbf is
  
begin
  
end tom;



entity vvtbi is
  port (wgkej : buffer integer; eciiuo : in real);
end vvtbi;

library ieee;
use ieee.std_logic_1164.all;

architecture mdo of vvtbi is
  signal y : std_logic_vector(2 to 0);
  signal g : boolean_vector(3 to 0);
  signal pklueukom : real;
  signal opu : std_logic_vector(2 to 0);
  signal iw : boolean_vector(3 to 0);
  signal khabw : real;
  signal ew : std_logic_vector(2 to 0);
  signal zbz : real;
  signal hdpwffhze : boolean_vector(3 to 0);
  signal xzozvuzb : std_logic_vector(2 to 0);
  signal sodzqbta : boolean_vector(3 to 0);
  signal suxc : real;
begin
  s : entity work.xkbf
    port map (srickcd => suxc, gvjvolwd => sodzqbta, xnju => suxc, vniuynayh => xzozvuzb);
  kz : entity work.xkbf
    port map (srickcd => eciiuo, gvjvolwd => hdpwffhze, xnju => zbz, vniuynayh => ew);
  ioqoj : entity work.xkbf
    port map (srickcd => khabw, gvjvolwd => iw, xnju => khabw, vniuynayh => opu);
  nrqpgtb : entity work.xkbf
    port map (srickcd => pklueukom, gvjvolwd => g, xnju => pklueukom, vniuynayh => y);
end mdo;



entity wof is
  port (bvrctblgap : out time_vector(1 downto 0));
end wof;

library ieee;
use ieee.std_logic_1164.all;

architecture gfhnrlz of wof is
  signal tsdfhg : real;
  signal qc : boolean_vector(3 to 0);
  signal ttgxuqzf : std_logic_vector(2 to 0);
  signal tgdiavrqf : real;
  signal fciwdcqpd : boolean_vector(3 to 0);
  signal hooceuqto : real;
  signal b : real;
  signal tzljlnticj : integer;
begin
  awjv : entity work.vvtbi
    port map (wgkej => tzljlnticj, eciiuo => b);
  qch : entity work.xkbf
    port map (srickcd => hooceuqto, gvjvolwd => fciwdcqpd, xnju => tgdiavrqf, vniuynayh => ttgxuqzf);
  zkiuspg : entity work.xkbf
    port map (srickcd => b, gvjvolwd => qc, xnju => tsdfhg, vniuynayh => ttgxuqzf);
end gfhnrlz;



-- Seed after: 15682411333219370760,6329330932550885447
