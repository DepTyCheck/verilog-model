-- Seed: 11723293691921880209,2511821214772927453

entity duf is
  port (tvd : buffer real; qsbpokxqk : linkage bit; cbsbd : out real_vector(4 downto 1));
end duf;

architecture u of duf is
  
begin
  -- Single-driven assignments
  cbsbd <= (8#4.10651#, 04.4, 1.3_3_3, 2#00000.1#);
end u;

entity dtid is
  port (jkbdtd : inout boolean; rmtix : buffer bit);
end dtid;

architecture eneukoqgz of dtid is
  
begin
  -- Single-driven assignments
  rmtix <= '0';
end eneukoqgz;

entity mw is
  port (qfm : buffer integer_vector(3 downto 4));
end mw;

architecture dt of mw is
  signal cxnouci : real_vector(4 downto 1);
  signal ay : bit;
  signal pdhjl : real;
  signal gffkusotdn : real_vector(4 downto 1);
  signal bshglckkek : bit;
  signal tfthqzxgzg : real;
  signal waocp : real_vector(4 downto 1);
  signal z : bit;
  signal wnifvdeph : real;
  signal zyjilh : real_vector(4 downto 1);
  signal vychbwsvcx : bit;
  signal twlcxmlzxd : real;
begin
  q : entity work.duf
    port map (tvd => twlcxmlzxd, qsbpokxqk => vychbwsvcx, cbsbd => zyjilh);
  xgrfwzt : entity work.duf
    port map (tvd => wnifvdeph, qsbpokxqk => z, cbsbd => waocp);
  d : entity work.duf
    port map (tvd => tfthqzxgzg, qsbpokxqk => bshglckkek, cbsbd => gffkusotdn);
  ryejrg : entity work.duf
    port map (tvd => pdhjl, qsbpokxqk => ay, cbsbd => cxnouci);
  
  -- Single-driven assignments
  qfm <= (others => 0);
end dt;

library ieee;
use ieee.std_logic_1164.all;

entity jpsy is
  port (jh : buffer integer; ubqwqucou : inout std_logic_vector(1 downto 1); dv : inout real; ep : in integer);
end jpsy;

architecture v of jpsy is
  signal yn : real_vector(4 downto 1);
  signal bwsjd : bit;
  signal ld : real;
  signal od : real_vector(4 downto 1);
  signal awbug : bit;
  signal eeezwn : real;
  signal iqsgnwlo : integer_vector(3 downto 4);
  signal rso : real_vector(4 downto 1);
  signal dkwv : bit;
  signal oscczmjbij : real;
begin
  vzkb : entity work.duf
    port map (tvd => oscczmjbij, qsbpokxqk => dkwv, cbsbd => rso);
  szbxm : entity work.mw
    port map (qfm => iqsgnwlo);
  jsdipjfg : entity work.duf
    port map (tvd => eeezwn, qsbpokxqk => awbug, cbsbd => od);
  hfymzhy : entity work.duf
    port map (tvd => ld, qsbpokxqk => bwsjd, cbsbd => yn);
  
  -- Single-driven assignments
  dv <= 16#738E.7_C#;
  jh <= ep;
  
  -- Multi-driven assignments
  ubqwqucou <= "X";
  ubqwqucou <= (others => 'U');
end v;



-- Seed after: 15570679557721119251,2511821214772927453
