-- Seed: 1631687262684684574,4245627776430562977

entity lgtpaprni is
  port (zgnuz : inout string(4 to 3));
end lgtpaprni;

architecture lo of lgtpaprni is
  
begin
  -- Single-driven assignments
  zgnuz <= "";
end lo;

entity lx is
  port (g : out real; bzhenmdsc : out integer);
end lx;

architecture n of lx is
  signal qymqd : string(4 to 3);
  signal vrwuhnf : string(4 to 3);
  signal adwrc : string(4 to 3);
begin
  lmfwbwpyci : entity work.lgtpaprni
    port map (zgnuz => adwrc);
  z : entity work.lgtpaprni
    port map (zgnuz => vrwuhnf);
  fbivgofx : entity work.lgtpaprni
    port map (zgnuz => qymqd);
  
  -- Single-driven assignments
  bzhenmdsc <= 4;
  g <= g;
end n;



-- Seed after: 16572707563978331331,4245627776430562977
