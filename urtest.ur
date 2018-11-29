signature USER = sig
    type id
    type password
    val id_read : read id
    val pass_read : read password
    val id_show : show id
    val login : { Id : id, Password : password } -> transaction bool
    val whoami : transaction (option id)
    val register : { Id : id, Password : password } -> transaction unit
end

functor MakeUser(M : sig type id
                         type password
                         val id_read : read id
                         val pass_read : read password
                         val id_show : show id
                         val inj_id : sql_injectable id
                         val inj_prim_id : sql_injectable_prim id
                         val inj_pass : sql_injectable password
                    end) : USER = struct
    type id = M.id
    type password = M.password
    val id_read = _
    val pass_read = _
    val id_show = _
    table user : { Id : id, Password : password }
                     PRIMARY KEY Id
    cookie c : { Id : id, Password : password }
    fun login r =
        b <- oneRowE1 (SELECT COUNT( * ) > 0
                       FROM user
                       WHERE user.Id = {[r.Id]}
                         AND user.Password = {[r.Password]});
        if b then
            setCookie c { Value = r, Expires = None, Secure = False };
            return True
        else return False
    val whoami =
        cc <- getCookie c;
        case cc of
            None => return None
          | Some r =>
            b <- oneRowE1 (SELECT COUNT( * ) > 0
                           FROM user
                           WHERE user.Id = {[r.Id]}
                             AND user.Password = {[r.Password]});
            if b then
                return (Some r.Id)
            else
                return None
    val register r =
        dml (INSERT INTO user(Id,Password) VALUES ({[r.Id]},{[r.Password]}))
end

structure User = MakeUser(struct
                              type id = string
                              type password = string
                          end)

fun main () =
    me <- User.whoami;
    case me of
        None => login "!Please log in"
      | Some me =>
        return <xml><body>
          <h1>Logged in as : {cdata (show me)}</h1>
        </body></xml>
and login msg =
    return <xml><body>
      <form>
        <p>{[msg]}</p>
        <textbox{#Id}/>
        <textbox{#Password}/>
        <submit action={signin}/><a link={register()}>Register</a>
      </form>
    </body></xml>
and signin r =
    success <- User.login { Id = readError r.Id, Password = readError r.Password };
    if success then main ()
    else login "Auth failed"
and register () =
    return <xml><body>
      <form>
        Username: <textbox{#Id}/><br/>
        Password: <textbox{#Password}/><br/>
        <submit action={signup}/>
      </form>
    </body></xml>
and signup r =
    User.register { Id = readError r.Id, Password = readError r.Password };
    login "Now please login"
