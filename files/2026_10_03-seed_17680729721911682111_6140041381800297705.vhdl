-- Seed: 17680729721911682111,6140041381800297705

entity qorjtlqfam is
  port (wazgvmqc : out real; mcs : inout real);
end qorjtlqfam;

architecture r of qorjtlqfam is
  
begin
  
end r;

entity dbdbjw is
  port (vx : linkage boolean_vector(4 to 4));
end dbdbjw;

architecture kmnydip of dbdbjw is
  signal ytdg : real;
  signal pxqdx : real;
  signal lo : real;
  signal uw : real;
  signal diy : real;
  signal m : real;
begin
  mmfmtd : entity work.qorjtlqfam
    port map (wazgvmqc => m, mcs => diy);
  ohozbe : entity work.qorjtlqfam
    port map (wazgvmqc => uw, mcs => lo);
  srwgk : entity work.qorjtlqfam
    port map (wazgvmqc => pxqdx, mcs => ytdg);
end kmnydip;

library ieee;
use ieee.std_logic_1164.all;

entity thpkvdm is
  port (j : in time; vzblas : buffer string(3 downto 1); gqy : buffer real; uzgl : linkage std_logic_vector(4 downto 4));
end thpkvdm;

architecture fifm of thpkvdm is
  signal nreyodpwr : real;
  signal pcn : real;
  signal ipraa : real;
  signal hltedahvb : boolean_vector(4 to 4);
begin
  cuu : entity work.dbdbjw
    port map (vx => hltedahvb);
  js : entity work.qorjtlqfam
    port map (wazgvmqc => ipraa, mcs => pcn);
  nwt : entity work.qorjtlqfam
    port map (wazgvmqc => nreyodpwr, mcs => gqy);
  
  -- Single-driven assignments
  vzblas <= vzblas;
end fifm;

entity ajhkyakho is
  port (uqdhhyju : in time);
end ajhkyakho;

library ieee;
use ieee.std_logic_1164.all;

architecture iioagjwbb of ajhkyakho is
  signal sdbzifqpt : real;
  signal rffmakor : real;
  signal bmbdcp : std_logic_vector(4 downto 4);
  signal z : real;
  signal pcud : string(3 downto 1);
  signal lfoguj : real;
  signal wakx : real;
begin
  d : entity work.qorjtlqfam
    port map (wazgvmqc => wakx, mcs => lfoguj);
  uytrl : entity work.thpkvdm
    port map (j => uqdhhyju, vzblas => pcud, gqy => z, uzgl => bmbdcp);
  suvpd : entity work.qorjtlqfam
    port map (wazgvmqc => rffmakor, mcs => sdbzifqpt);
  
  -- Multi-driven assignments
  bmbdcp <= bmbdcp;
  bmbdcp <= bmbdcp;
end iioagjwbb;



-- Seed after: 9886787808211399036,6140041381800297705
