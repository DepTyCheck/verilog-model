-- Seed: 3741388556980248582,5805648483995786113

entity pyzgylli is
  port (hyxqo : out bit_vector(4 to 2); nufj : out time; ocbhntxlvs : linkage real);
end pyzgylli;

architecture dtenxii of pyzgylli is
  
begin
  -- Single-driven assignments
  hyxqo <= (others => '0');
  nufj <= 2#0.1# ps;
end dtenxii;

entity yogthp is
  port (guouqieogf : out time_vector(3 downto 2); rwmaeajt : buffer time_vector(3 to 2));
end yogthp;

architecture ekyg of yogthp is
  signal ecezt : real;
  signal uyyexjfzs : time;
  signal a : bit_vector(4 to 2);
  signal icjrufxgx : real;
  signal pkl : time;
  signal kfnkb : bit_vector(4 to 2);
  signal cbwmmtpzb : real;
  signal icxr : time;
  signal rljpqmhg : bit_vector(4 to 2);
  signal xvcmbpet : real;
  signal t : time;
  signal uh : bit_vector(4 to 2);
begin
  y : entity work.pyzgylli
    port map (hyxqo => uh, nufj => t, ocbhntxlvs => xvcmbpet);
  aiaxlvernv : entity work.pyzgylli
    port map (hyxqo => rljpqmhg, nufj => icxr, ocbhntxlvs => cbwmmtpzb);
  bl : entity work.pyzgylli
    port map (hyxqo => kfnkb, nufj => pkl, ocbhntxlvs => icjrufxgx);
  tzh : entity work.pyzgylli
    port map (hyxqo => a, nufj => uyyexjfzs, ocbhntxlvs => ecezt);
end ekyg;



-- Seed after: 15180368250673858313,5805648483995786113
