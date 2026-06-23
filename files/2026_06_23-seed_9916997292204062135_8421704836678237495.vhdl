-- Seed: 9916997292204062135,8421704836678237495

library ieee;
use ieee.std_logic_1164.all;

entity ipiysswv is
  port ( jthqqsbi : out std_logic_vector(2 downto 2)
  ; ow : in std_logic_vector(1 downto 0)
  ; ogabik : linkage string(5 to 5)
  ; jskb : inout bit_vector(3 downto 3)
  );
end ipiysswv;

architecture zccyuxhor of ipiysswv is
  
begin
  -- Single-driven assignments
  jskb <= (others => '0');
  
  -- Multi-driven assignments
  jthqqsbi <= "-";
  jthqqsbi <= "X";
  jthqqsbi <= "1";
  jthqqsbi <= (others => '-');
end zccyuxhor;

entity qizta is
  port (dnpdexspfe : inout time);
end qizta;

library ieee;
use ieee.std_logic_1164.all;

architecture vwnzahn of qizta is
  signal bubersqjq : bit_vector(3 downto 3);
  signal ujytdmzi : string(5 to 5);
  signal scfghb : std_logic_vector(1 downto 0);
  signal aguqbhvn : bit_vector(3 downto 3);
  signal neuewks : string(5 to 5);
  signal cishwjdad : std_logic_vector(1 downto 0);
  signal ynsyexfr : std_logic_vector(2 downto 2);
  signal oyxncddyh : bit_vector(3 downto 3);
  signal gdupod : string(5 to 5);
  signal v : std_logic_vector(1 downto 0);
  signal nladlq : std_logic_vector(2 downto 2);
  signal bjoxu : bit_vector(3 downto 3);
  signal npra : string(5 to 5);
  signal rgcgek : std_logic_vector(1 downto 0);
  signal hsakwmo : std_logic_vector(2 downto 2);
begin
  gmb : entity work.ipiysswv
    port map (jthqqsbi => hsakwmo, ow => rgcgek, ogabik => npra, jskb => bjoxu);
  qo : entity work.ipiysswv
    port map (jthqqsbi => nladlq, ow => v, ogabik => gdupod, jskb => oyxncddyh);
  igdz : entity work.ipiysswv
    port map (jthqqsbi => ynsyexfr, ow => cishwjdad, ogabik => neuewks, jskb => aguqbhvn);
  toytkezf : entity work.ipiysswv
    port map (jthqqsbi => hsakwmo, ow => scfghb, ogabik => ujytdmzi, jskb => bubersqjq);
  
  -- Single-driven assignments
  dnpdexspfe <= 40420 ns;
end vwnzahn;

entity puqobeziu is
  port (nwgw : in time_vector(0 downto 2); myqrpblf : buffer time);
end puqobeziu;

library ieee;
use ieee.std_logic_1164.all;

architecture m of puqobeziu is
  signal trnovsm : bit_vector(3 downto 3);
  signal ihbnbqxzc : string(5 to 5);
  signal pyyad : std_logic_vector(1 downto 0);
  signal ujfqerfer : std_logic_vector(2 downto 2);
begin
  mqnhssc : entity work.ipiysswv
    port map (jthqqsbi => ujfqerfer, ow => pyyad, ogabik => ihbnbqxzc, jskb => trnovsm);
  
  -- Single-driven assignments
  myqrpblf <= 4020.03103 ps;
  
  -- Multi-driven assignments
  pyyad <= "L-";
  ujfqerfer <= (others => 'H');
end m;

entity rxficzofa is
  port (lkosj : inout integer; feqkx : buffer bit; dfm : buffer integer_vector(3 to 3));
end rxficzofa;

architecture elnxncz of rxficzofa is
  signal kipwe : time;
  signal vq : time_vector(0 downto 2);
begin
  ronvw : entity work.puqobeziu
    port map (nwgw => vq, myqrpblf => kipwe);
  
  -- Single-driven assignments
  dfm <= (others => 8#3_3#);
  feqkx <= '1';
end elnxncz;



-- Seed after: 11835301269244537602,8421704836678237495
