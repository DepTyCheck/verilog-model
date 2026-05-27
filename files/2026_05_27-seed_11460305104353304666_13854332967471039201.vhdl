-- Seed: 11460305104353304666,13854332967471039201



entity olyn is
  port (ydtlinkiv : in time; zdza : in real; efcqmiu : buffer real);
end olyn;



architecture ufrqxwy of olyn is
  
begin
  
end ufrqxwy;



entity xlt is
  port (vx : inout time; pdoxhhcr : buffer time);
end xlt;



architecture adowfm of xlt is
  signal rezj : real;
  signal nmpzmsw : real;
  signal d : real;
  signal cz : real;
begin
  fwl : entity work.olyn
    port map (ydtlinkiv => pdoxhhcr, zdza => cz, efcqmiu => cz);
  pky : entity work.olyn
    port map (ydtlinkiv => pdoxhhcr, zdza => d, efcqmiu => nmpzmsw);
  cmbkigmt : entity work.olyn
    port map (ydtlinkiv => vx, zdza => rezj, efcqmiu => rezj);
end adowfm;



entity cvgq is
  port (ooidysf : in time_vector(1 to 4); mwnefjl : inout time);
end cvgq;



architecture wyhvba of cvgq is
  signal sgla : real;
  signal nsohwvpwra : real;
  signal kwqdta : real;
  signal cqmd : time;
begin
  batairm : entity work.olyn
    port map (ydtlinkiv => cqmd, zdza => kwqdta, efcqmiu => nsohwvpwra);
  bqvu : entity work.olyn
    port map (ydtlinkiv => mwnefjl, zdza => nsohwvpwra, efcqmiu => sgla);
  tebbxh : entity work.olyn
    port map (ydtlinkiv => mwnefjl, zdza => nsohwvpwra, efcqmiu => kwqdta);
end wyhvba;



-- Seed after: 9852239842611949831,13854332967471039201
