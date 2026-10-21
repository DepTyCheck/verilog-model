-- Seed: 9089549922153198079,17219977430298397867

entity ivjvmechy is
  port (evwpgyfcaa : buffer bit; tl : inout bit);
end ivjvmechy;

architecture xuvhj of ivjvmechy is
  
begin
  -- Single-driven assignments
  tl <= '1';
  evwpgyfcaa <= '0';
end xuvhj;

entity qfulrme is
  port (jpvgpsxn : linkage time);
end qfulrme;

architecture hwkrciw of qfulrme is
  signal grhkfurbrl : bit;
  signal rnnmxm : bit;
begin
  pqjje : entity work.ivjvmechy
    port map (evwpgyfcaa => rnnmxm, tl => grhkfurbrl);
end hwkrciw;

library ieee;
use ieee.std_logic_1164.all;

entity uvqewmo is
  port (rii : out bit_vector(1 downto 1); kpr : buffer std_logic_vector(1 to 0); bw : in std_logic_vector(0 downto 1); qzd : buffer time);
end uvqewmo;

architecture nkdihugwyi of uvqewmo is
  signal gqeklzb : time;
begin
  gbsuq : entity work.qfulrme
    port map (jpvgpsxn => qzd);
  bkdnk : entity work.qfulrme
    port map (jpvgpsxn => gqeklzb);
  
  -- Multi-driven assignments
  kpr <= bw;
end nkdihugwyi;

entity dkfqnicmf is
  port (cs : out integer; mhk : in real_vector(1 to 4); qdtqmr : linkage integer);
end dkfqnicmf;

library ieee;
use ieee.std_logic_1164.all;

architecture urogfgbz of dkfqnicmf is
  signal llpeyryz : time;
  signal msxxsqy : bit_vector(1 downto 1);
  signal ubuermhm : bit;
  signal z : bit;
  signal cjhxixq : time;
  signal oymoeld : std_logic_vector(0 downto 1);
  signal gqnz : std_logic_vector(0 downto 1);
  signal cmnpnx : bit_vector(1 downto 1);
  signal n : bit;
  signal pm : bit;
begin
  yexxc : entity work.ivjvmechy
    port map (evwpgyfcaa => pm, tl => n);
  cwbqrenkm : entity work.uvqewmo
    port map (rii => cmnpnx, kpr => gqnz, bw => oymoeld, qzd => cjhxixq);
  idkvsxcqhx : entity work.ivjvmechy
    port map (evwpgyfcaa => z, tl => ubuermhm);
  tyvs : entity work.uvqewmo
    port map (rii => msxxsqy, kpr => gqnz, bw => gqnz, qzd => llpeyryz);
  
  -- Single-driven assignments
  cs <= 3_4_4_2;
  
  -- Multi-driven assignments
  gqnz <= "";
  oymoeld <= oymoeld;
end urogfgbz;



-- Seed after: 5899250024328886007,17219977430298397867
