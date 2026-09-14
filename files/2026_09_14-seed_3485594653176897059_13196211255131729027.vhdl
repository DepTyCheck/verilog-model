-- Seed: 3485594653176897059,13196211255131729027

entity bahfus is
  port (zoscoazw : in time; ousjlw : inout time);
end bahfus;

architecture vxgjsfun of bahfus is
  
begin
  
end vxgjsfun;

library ieee;
use ieee.std_logic_1164.all;

entity kyhwa is
  port (bvxdxfiwp : out std_logic_vector(2 downto 4); yc : linkage severity_level; gadcn : in real; oakkibn : in std_logic_vector(2 downto 2));
end kyhwa;

architecture v of kyhwa is
  signal wzjoadlc : time;
  signal cyfzcajq : time;
begin
  lzkhyhklqo : entity work.bahfus
    port map (zoscoazw => cyfzcajq, ousjlw => cyfzcajq);
  e : entity work.bahfus
    port map (zoscoazw => cyfzcajq, ousjlw => wzjoadlc);
  
  -- Multi-driven assignments
  bvxdxfiwp <= "";
  bvxdxfiwp <= "";
end v;

entity tslezfvls is
  port (ma : in character; qqfd : out real; prfepozg : out bit);
end tslezfvls;

library ieee;
use ieee.std_logic_1164.all;

architecture yifhfvpgfn of tslezfvls is
  signal vgqgew : real;
  signal tmzruvsi : severity_level;
  signal uaqtond : std_logic_vector(2 downto 4);
  signal jfe : std_logic_vector(2 downto 2);
  signal cncph : severity_level;
  signal e : std_logic_vector(2 downto 4);
begin
  falcmq : entity work.kyhwa
    port map (bvxdxfiwp => e, yc => cncph, gadcn => qqfd, oakkibn => jfe);
  lfhbwp : entity work.kyhwa
    port map (bvxdxfiwp => uaqtond, yc => tmzruvsi, gadcn => vgqgew, oakkibn => jfe);
  
  -- Single-driven assignments
  qqfd <= qqfd;
  
  -- Multi-driven assignments
  e <= "";
end yifhfvpgfn;

entity egcijpoixj is
  port (zjf : buffer real_vector(2 to 3));
end egcijpoixj;

architecture askceit of egcijpoixj is
  signal xobgq : time;
  signal zxrii : time;
  signal bktzp : bit;
  signal lelmc : real;
  signal gzazbrbqb : character;
begin
  ndrhp : entity work.tslezfvls
    port map (ma => gzazbrbqb, qqfd => lelmc, prfepozg => bktzp);
  d : entity work.bahfus
    port map (zoscoazw => zxrii, ousjlw => xobgq);
  
  -- Single-driven assignments
  zjf <= zjf;
end askceit;



-- Seed after: 16753029107587939815,13196211255131729027
