-- Seed: 636650251590343437,3687118713772291287

entity povqkh is
  port (ltrttlanh : inout bit; jxxmxojz : out real; sgvdvk : linkage time_vector(2 downto 1));
end povqkh;

architecture fcmcc of povqkh is
  
begin
  
end fcmcc;

library ieee;
use ieee.std_logic_1164.all;

entity ctowafz is
  port (rps : out std_logic_vector(1 to 0));
end ctowafz;

architecture jrjsz of ctowafz is
  signal hvhs : time_vector(2 downto 1);
  signal ox : real;
  signal xhxfz : bit;
  signal kybegqhimh : time_vector(2 downto 1);
  signal x : real;
  signal fywpija : bit;
  signal lcjqyf : time_vector(2 downto 1);
  signal fbpxgkf : real;
  signal sg : bit;
  signal tbakrkzsq : time_vector(2 downto 1);
  signal a : real;
  signal yugn : bit;
begin
  ijza : entity work.povqkh
    port map (ltrttlanh => yugn, jxxmxojz => a, sgvdvk => tbakrkzsq);
  qjfrimbds : entity work.povqkh
    port map (ltrttlanh => sg, jxxmxojz => fbpxgkf, sgvdvk => lcjqyf);
  c : entity work.povqkh
    port map (ltrttlanh => fywpija, jxxmxojz => x, sgvdvk => kybegqhimh);
  ibxzuca : entity work.povqkh
    port map (ltrttlanh => xhxfz, jxxmxojz => ox, sgvdvk => hvhs);
  
  -- Multi-driven assignments
  rps <= (others => '0');
end jrjsz;

entity zozujie is
  port (itablb : out bit; ndhhn : out time);
end zozujie;

library ieee;
use ieee.std_logic_1164.all;

architecture ivpvtgqkz of zozujie is
  signal ophqgjvp : std_logic_vector(1 to 0);
  signal vd : time_vector(2 downto 1);
  signal adnndsfs : real;
  signal lnkqb : time_vector(2 downto 1);
  signal fri : real;
  signal nynxcqma : bit;
begin
  jxrz : entity work.povqkh
    port map (ltrttlanh => nynxcqma, jxxmxojz => fri, sgvdvk => lnkqb);
  fniljeou : entity work.povqkh
    port map (ltrttlanh => itablb, jxxmxojz => adnndsfs, sgvdvk => vd);
  nkzaohdp : entity work.ctowafz
    port map (rps => ophqgjvp);
  
  -- Single-driven assignments
  ndhhn <= 2#00.0# ns;
  
  -- Multi-driven assignments
  ophqgjvp <= "";
  ophqgjvp <= "";
end ivpvtgqkz;

library ieee;
use ieee.std_logic_1164.all;

entity h is
  port (hdez : buffer std_logic; tlvie : buffer time);
end h;

library ieee;
use ieee.std_logic_1164.all;

architecture hgbgwz of h is
  signal jccw : bit;
  signal qkqudr : time_vector(2 downto 1);
  signal ommd : real;
  signal rvxpmjcj : bit;
  signal ynv : std_logic_vector(1 to 0);
  signal tozqw : std_logic_vector(1 to 0);
begin
  alzkw : entity work.ctowafz
    port map (rps => tozqw);
  ubw : entity work.ctowafz
    port map (rps => ynv);
  spknd : entity work.povqkh
    port map (ltrttlanh => rvxpmjcj, jxxmxojz => ommd, sgvdvk => qkqudr);
  cwomx : entity work.zozujie
    port map (itablb => jccw, ndhhn => tlvie);
  
  -- Multi-driven assignments
  hdez <= '1';
  hdez <= '0';
end hgbgwz;



-- Seed after: 9534919116389660802,3687118713772291287
